package zzb.domain

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import shapeless.{::, HNil}
import spray.http.StatusCodes._
import spray.json.{DefaultJsonProtocol, JsArray}
import zzb.datatype._
import zzb.domain.directive._
import zzb.rest.{StatusCode, _}
import zzb.rest.directives.{MethodDirectives, OnCompleteFutureMagnet, OnSuccessFutureMagnet}
import zzb.storage._
import zzb.util._

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

/**
 * Created by Simon on 2014/4/16
 *
 * 当一个 MultiQuoteActor 被创建时，有两种可能
 * 1. 用户请求创建一个新DomainActor  (isNewCreate = true)
 * 2. 用户需要访问一个已存在的DomainActor (isNewCreate = false)
 *
 * 如果是第二种情况，会立即装载这个DomainActor，
 * 如果不存在，该Actor收到的所有请求都会响应404
 * 如果存在，对每一个请求要检查操作者，如果是用户操作，不是该多方的所有人响应403（禁止访问）
 * 如果是业管操作则通过（先不检查业管的具体权限）
 *
 * URL段说明
 * 1.   /latest/{/...}                    【Get】          对最新版本的操作请求，一般只有Get动作，不支持Put修改
 * 2.   /ver/{versionNum}/{/...}          【Get】          对指定版本的操作请求，也是只读(Get)
 * 3.   /versions                         【Get】          获得版本列表
 * 4.   /alter{/...}                      【Put/Delete】   直接修改指定路径的数据（会将变更会话的 “创建”、“修改”、“提交”一次完成
 * 5.   /alter{/...}                      【Post】         申请修改指定路径,返回用于修改的URL(包含一个修改会话码，alterSid),如果已经有人在修改，返回修改者的工号或业管号
 * /alter/{alterSid}{/...}                【Get】          正在修改的版本的数据，包含已经修改的内容
 * /alter/{alterSid}{/...}                【Put】          修改文档指定路径的内容
 * /alter/{alterSid}{/...}                【Delete】       删除文档指定路径的内容
 * /alter/{alterSid}                      【Delete】       放弃修改内容
 * /alter/{alterSid}                      【Post】         提交本次修改，alterSid将会失效，/latest/... 将会得到最新版本的内容
 * /alter/{alterSid}?action={actionName}  【Post】         提交本次修改，同时执行处理动作 . alterSid将会失效，/latest/... 将会得到最新版本的内容
 * 6.   /action/{actionName}              【Post】         执行动作，如果有未提交的变更会话，会报409错误
 * /action/{actionName}?force=true        【Post】         强制执行动作，所有有未提交的变更会话会被废弃
 * 7.                                     【Delete】 将文档从缓存中清理，并将actor销毁（并没有从缓存中删除文档）
 *
 */
trait DomainActor[K, KT <: DataType[K], T <: TStorable[K, KT]] extends RestServiceActor with DocProcessor[K, KT, T]
with AuthorizeDirectives with DomainDirectives with DomainLogging {

  val docType = specStorage.storage.driver.docType

  /**
   * 领域对象主键
   */
  val domId: K

  private val hlog = HeritLog("dom")

  //获取命名超时
  val getTimeout: String => Timeout = str => Timeout(2000.milli)

  /**
   * 创建时的所有者
   */
  val createOwner: Option[String]
  /**
   * 是否是新创建
   */
  val isNewCreate: Boolean

  val domainType: T

  def docState: Int

  def getOwnerFromDoc(doc: T#Pack): String

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    hlog(log.error(reason, "preRestart"))
    super.preRestart(reason, message)
  }

  override def postStop(): Unit = {
    hlog(log.info("domain actor stoped!"))
  }

  override def receive: Receive = msgReceive orElse restReceive

  protected def restReceive = runRoute(domainRoute)

  def domainRoute: Route =
    pathEndOrSingleSlash {
      MethodDirectives.delete {
        onComplete(specStorage.release()) {
          case Success(Some(d)) =>
            self ! PoisonPill
            complete(s"Actor $domId Released")
          case Success(None) =>
            self ! PoisonPill
            complete(s"Actor $domId Released")
          case Failure(e) =>
            self ! PoisonPill
            complete(InternalServerError, s"Actor已销毁，但从缓存清理失败：${e.getMessage}")
        }
      } ~ docFutureRoute(latest)
    } ~
      pathPrefix("ver" / IntNumber) {
        verNum: Int =>
          docFutureRoute(load(verNum))
      } ~ pathPrefix(Segment) {
      case "latest" => docFutureRoute(latest)
      case "alter" => alterRoute
      case "versions" => versionsRoute
      case "action" => actionRoute
      case _ => reject
    }

  import zzb.domain.DomainActor._

  private var initWaits = List[ActorRef]()

  private var ctxWhenIniting = List[RestReqContext]()

  import zzb.domain.DocLoadStatus._

  private var docLoadStatus = DocUnload

  private var initResult: InitDocOver[_] = _


  protected def msgReceive: Receive = {
    case StartInit =>
      docLoadStatus match {
        case DocUnload =>
          docLoadStatus = DocLoading
          initLoadDoc
          if (sender() != self) initWaits = sender :: initWaits
        case DocLoading =>
          if (sender() != self) initWaits = sender :: initWaits
        case _ =>
          if (initResult != null) sender ! initResult
      }
    case res: InitDocOver[_] =>
      res match {
        case m@InitDocOver(Right(doc)) =>
          docLoadStatus = DocLoaded
          hlog(sysopt)(log.info(s"doc loaded!"))
        case m@InitDocOver(Left(e@RestException(_))) =>
          docLoadStatus = DocLoadFailed
          hlog(sysopt)(log.info(e.err.value))
          self ! PoisonPill
        case m@InitDocOver(Left(e)) =>
          docLoadStatus = DocLoadFailed
          hlog(sysopt)(log.error(s"doc load failed!"))
          self ! PoisonPill
      }
      initResult = res
      //initDocOver 函数需要在 initWaits.map(_ ! res) 之前被调用
      initDocOver(res.asInstanceOf[InitDocOver[T]])
      if (initWaits.size > 0) {
        initWaits.map(_ ! res)
        initWaits = Nil
      }
      if (ctxWhenIniting.size > 0) {
        ctxWhenIniting.map(self ! _)
        ctxWhenIniting = Nil
      }
    case AlterSessionsChanged(sessions, Some(oldDoc), Some(newDoc), change) =>
      as = sessions
      DocChanged(oldDoc, newDoc, change)

    case AlterSessionsChanged(sessions, _, _, _) => as = sessions

    case ctx: RestReqContext if docLoadStatus == DocLoading =>
      ctxWhenIniting = ctx :: ctxWhenIniting

  }

  private def initLoadDoc() = {
    hlog(sysopt)(log.info("fsm starting by load latest doc ... "))
    latest.onComplete {
      case Success(Some(mq)) =>
        self ! InitDocOver(Right(mq))
      //hlog(sysopt)(log.info(s"${if (isNewCreate) "create" else "load"} [${docType.t_memo_}] success!"))
      case Success(None) =>
        //        self ! InitDocOver(Left(new ResourceNotFound(domId.toString)))
        self ! InitDocOver(Left(new RestException(StatusCodes.NotFound.copy()(reason = s"request resource ${domId.toString} not found!", defaultMessage = StatusCodes.NotFound.defaultMessage))))
      //hlog(sysopt)(log.warning(s"${if (isNewCreate) "create" else "load"} [${docType.t_memo_}] failed!"))
      case Failure(e) =>
        self ! InitDocOver(Left(e))
      //hlog(sysopt)(log.error(s"${if (isNewCreate) "create" else "load"} [${docType.t_memo_}] error!"))
    }
  }

  def initDocOver(res: InitDocOver[T]) {}

  def startInit() {
    self ! StartInit
  }

  def alterVerify(curDoc: T#Pack, path: StructPath, fieldValue: Option[ValuePack[Any]],
                  operator: AuthorizedOperator): Either[(StatusCode, String), Option[ValuePack[Any]]]

  def alterPathVerify(path: StructPath, operator: AuthorizedOperator): Option[String] = None

  def submitAlter(doc: T#Pack, operator: AuthorizedOperator, seq: Int,
                  action: Option[String] = None, params: Map[String, String] = Map[String, String]()): Future[(StatusCode, ActionResult)] = {
    if (action.isDefined)
      hlog(operator)(log.info("alter session {} submitting with action '{}' ...", seq, action.get))
    else
      hlog(operator)(log.info("alter session {} submitting... ", seq))

    val p = Promise[(StatusCode, ActionResult)]()

    val f1 = save(doc).map { savedDocOpt =>
      val savedDoc = savedDocOpt.get
      hlog(log.info("saved! ver = {} revise = {}", savedDoc.version, savedDoc.revise))
      (StatusCodes.OK, ActionResult(0, "AlterOK", VersionRevise(savedDoc.version, savedDoc.revise)))
    }
    val res_f = if (action.isDefined) {
      for {
        v1 <- f1
        v2 <- self.ask(Action(action.get, operator, params, None))(getTimeout(s"action-${action.get}"))
      } yield v2
    }
    else f1

    res_f.onComplete {
      case Success(v) => p.success(anyToActionResult(v))
      case Failure(e) => p.failure(e)
    }
    p.future

  }

  val owner: Future[Option[String]] = latest.map {
    case None => None
    case Some(v: T#Pack) => Some(getOwnerFromDoc(v))
  }

  protected def DocChanged(oldDoc: T#Pack, newDoc: T#Pack, change: List[StructPath]) {
    val oldVer = oldDoc.version
    val newVer = newDoc.version
    val invokedPath = scala.collection.mutable.Set[StructPath]() //记录已经激活的监听器，确保监听器不会被调用多次
    change.foreach { changedPath =>
      alterMonitors.foreach { monitor =>
        if (!invokedPath.contains(monitor.path) && (
          monitor.path.contains(changedPath) || changedPath.contains(monitor.path)
          )
        ) {
          val notifyPath = if (monitor.onlyChangedPath) changedPath else monitor.path
          val oldData = notifyPath.getDomainData(oldDoc)
          val newData = notifyPath.getDomainData(newDoc)
          invokedPath.add(monitor.path)
          if (oldData != newData) {
            monitor.handler(notifyPath, newData, newVer, oldData, oldVer)
          }
        }
      }
    }
  }

  implicit def dtToPath(dt: DataType[Any]): Path = Path(dt.path)

  implicit def dtFunToPath(dtFun: () => DataType[Any]): Path = Path(dtFun().path)

  implicit def dtIdxToPath(dtIdx: (DataType[Any], Int)): Path = Path(dtIdx._1.path + "/" + dtIdx._2)

  implicit def dtFunIdxToPath(dtFunIdx: (() => DataType[Any], Int)): Path = Path(dtFunIdx._1().path + "/" + dtFunIdx._2)

  implicit def dtKeyToPath(dtKey: (DataType[Any], String)): Path = Path(dtKey._1.path + "/" + dtKey._2)

  implicit def dtKeyFuncToPath(dtKey: (() => DataType[Any], String)): Path = Path(dtKey._1().path + "/" + dtKey._2)

  case class Path(value: String)

  type AlterMonitorHandler = (StructPath, Option[Any], Int, Option[Any], Int) => Unit

  case class AlterMonitor(path: StructPath, handler: AlterMonitorHandler,onlyChangedPath:Boolean)

  private var alterMonitors = List[AlterMonitor]()

  /**
   *
   * @param path 监控的路径
   * @param monitorHandler 数据变更的处理函数
   * @param notifyRealChangedPath 如果为 true ,monitorHandler 被调用时，传入的额路径是实际变更的路径；
   *                              如果为false 传入的路径为监控的路径
   */
  final def monitorAlter(path: StructPath, monitorHandler: AlterMonitorHandler,notifyRealChangedPath :Boolean = false): Unit =
    alterMonitors :+= AlterMonitor(path, monitorHandler,notifyRealChangedPath)

  var as = List[AlterSession]()

  val actionBuilder: ActionBuilder = null

  def actionRoute: Route =
    if (actionBuilder == null)
      reject
    else
      post {
        innerActionRoute(actionBuilder.actions.toMap)
      }

  private def innerActionRoute(actionDefs: Map[String, ActionDefine]): Route = {
    val routes = for ((actName, define) <- actionDefs) yield {
      define.entityType match {
        case None =>
          path(actName) {
            operator {
              opt =>
                if (supportAction(opt, actName, withEntity = false))
                  parameterMap { params =>
                    val action = new Action(actName, opt, params, None)
                    actionExeRouteCheckSessionConflict(action)
                  }
                else {
                  val msg = s"${if (opt.isManager) "manager" else "user"} can't execute action $actName"
                  hlog(opt)(log.warning(msg))
                  complete(Forbidden, ActionResult(docState, msg, VersionRevise(-1, -1)))
                }
            }
          }
        case Some(dt) =>
          path(actName) {
            operator {
              opt =>
                if (supportAction(opt, actName, withEntity = true))
                  entity(unpack(dt)) { pk =>
                    parameterMap { params =>
                      val action = new Action(actName, opt, params, Some(pk))
                      actionExeRouteCheckSessionConflict(action)
                    }
                  }
                else {
                  val msg = s"${if (opt.isManager) "manager" else "user"} can't execute action $actName"
                  hlog(opt)(log.warning(msg))
                  complete(Forbidden, ActionResult(docState, msg, VersionRevise(-1, -1)))
                }
            }
          }
      }
    }
    routes.reduce(_ ~ _) ~ path(Segment) {
      actName => complete(NotFound, s"action '$actName' not exist!")
    }
  }

  //检查是否有会话冲突
  def actionExeRouteCheckSessionConflict(a: Action): Route = {
    val params = a.params
    val force = params.contains("force") && params("force").toLowerCase == "true"

    (as.size, force) match {
      case (0, _) => clearSessionBeforeActionExeRoute(a)
      case (_, true) => clearSessionBeforeActionExeRoute(a)
      case (n, _) =>
        hlog(a.opt)(log.warning("refuse execute action '{}' for unsubmit({}) alter session ", a.name, n))
        val msg = JsArray(as.map(_.copy(seq = -1)).map(AlterSession.format.write))
        complete(Conflict, msg)
    }
  }

  def clearSessionBeforeActionExeRoute(a: Action): Route = {
    if (as.size > 0) {
      hlog(a.opt)(log.info("force close all({}) alter sessions ", as.size))
      onSuccess(context.child("alter").get.ask(AlterHalt)(500 milliseconds)) {
        case _ =>
          hlog(a.opt)(log.info("request execute action '{}'  ", a.name))
          actionExeRoute(a)
      }
    } else {
      hlog(a.opt)(log.info("request execute action '{}'  ", a.name))
      actionExeRoute(a)
    }
  }

  //实际启动Action执行
  def actionExeRoute(a: Action): Route =
    onComplete(self.ask(a)(getTimeout(s"action-${a.name}"))) {
      case Success(v) =>
        val res = anyToActionResult(v)
        complete(res._1, res._2)
      case Failure(e) => failWith(e)
    }


  protected def anyToActionResult(any: Any): (StatusCode, ActionResult) = any match {
    case as: ActionResult =>
      (OK, as)
    case (sc: StatusCode, ar: ActionResult) =>
      (sc, ar)
    case (sc: StatusCode, ae: ActionError) =>
      (sc, ActionResult(ae.intValue, ae.reason, VersionRevise(-1, -1)))
    case (sc: StatusCode, ar: String) =>
      (sc, ActionResult(-1, ar, VersionRevise(-1, -1)))
    case doc: T#Pack =>
      (OK, ActionResult(-1, "OK2", VersionRevise(doc.version, doc.revise)))
    case Some(doc: T#Pack) =>
      (OK, ActionResult(-1, "OK1", VersionRevise(doc.version, doc.revise)))
    case e: Throwable =>
      (InternalServerError, ActionResult(-1, e.getMessage, VersionRevise(-1, -1)))
    case _ =>
      (InternalServerError, ActionResult(-1, "Failed", VersionRevise(-1, -1)))
  }

  /**
   * 检查请求执行的动作是否存在，并且操作者是否有权限执行这个动作,并且满足数据实体要求。
   * 检查结果与状态无关。 通过了这个检查，只是表明命令存在且操作者具有执行这个命令的权
   * 限，但也有可能数据状态不允许执行这个命令。
   * @param opt 操作者
   * @param action 动作名称
   * @return
   */
  def supportAction(opt: AuthorizedOperator, action: String, withEntity: Boolean): Boolean =
    if (actionBuilder == null) false
    else {
      actionBuilder.actions.get(action) match {
        case Some(actDef) => //
          actDef.roleCheck(opt) && actDef.entityType.isDefined == withEntity
        case None => false
      }
    }

  def docFutureRoute(docFuture: Future[Option[T#Pack]]): Route = {
    val docNodeRoute: DomainRoute = {
      path =>
        get {
          onSuccess(docFuture) {
            case Some(d) =>
              if (path.inStructPath.size == 1) complete(d)
              else {
                path.getDomainData(d) match {
                  case Some(v) =>
                    complete(v)
                  case None =>
                    complete(NotFound)
                }
              }
            case None =>
              complete(NotFound)
          }
        }
    }
    handDoc(domainType, docNodeRoute)
  }

  def versionsRoute: Route = get {
    onSuccess(versions) {
      case vers: Seq[_] => complete(VersionInfos(vers.toList))
    }
  }

  def alterRoute: Route = ctx => {
    context.child("alter").getOrElse(
      context.actorOf(Props(new AlterManagerActor), "alter")
    ).forward(ctx)
  }

  case object AlterHalt

  case class AlterSessionsChanged(sessions: List[AlterSession], oldDoc: Option[T#Pack], newDoc: Option[T#Pack], change: List[StructPath])

  class AlterManagerActor extends RestServiceActor with AuthorizeDirectives {

    case class AlterOver(seq: Int, oldDoc: T#Pack, newDoc: Option[T#Pack] = None, change: List[StructPath] = Nil)

    def receive: Receive = rcv orElse runRoute(alterRoute)

    val random = new Random(java.lang.System.currentTimeMillis)
    var curAlterInfo: AlterSession = _
    val alterSessions = scala.collection.mutable.Map[Int, AlterSession]()

    /**
     * 生成下一个变更会话的序号
     * @return
     */
    private def nextSeq: Int = {
      val seq = random.nextInt().abs % 10000000
      if (!alterSessions.contains(seq)) seq else nextSeq
    }

    def childByName(name: String) = {
      context.child(name) match {
        case None => Left((NotFound, "not found"))
        case Some(act) => Right(act)
      }
    }

    def rcv: Receive = {
      case AlterOver(seq, oldDoc, newDoc, change) =>
        alterSessions.remove(seq)
        context.parent ! AlterSessionsChanged(alterSessions.values.toList, Some(oldDoc), newDoc, change)
      case AlterHalt =>
        val children = context.children
        context.actorOf(Props(new AlterTerminatorActor(children, sender())))
        children.foreach { c => c ! AlterHalt}
    }

    def alterRoute: Route =
      pathPrefixTest(IntNumber) { seq =>
        forwardChild(childByName, context)
      } ~ handDoc(domainType, alterRequestRoute)

    //申请启动一个变更会话，返回变更会话序号，或返回409冲突的错误
    def alterRequestRoute(path: StructPath): Route =
      operator {
        opt =>
          onSuccess(latest) {
            case None => reject
            case Some(doc) =>
              alterPathVerify(path, opt) match {
                case Some(msg) =>
                  ctx =>
                    ctx.complete(Forbidden, ActionResult(-1, msg, VersionRevise(doc.version, doc.revise)))
                case None =>
                  alterSessions.values.find(s => s.path.startsWith(path.toString) || path.toString.startsWith(s.path)) match {
                    case Some(s) =>
                      ctx =>
                        //变更请求冲突
                        val msg = AlterSession.format.write(s.copy(seq = -1)).toString()
                        hlog(opt)(log.info("alter request reused for conflict,request {},unsubmit {}", path, s.path))
                        ctx.complete(Conflict, ActionResult(docState, msg, VersionRevise(doc.version, doc.revise)))
                    case None => //没有冲突的变更会话，可以执行新的变更
                      path.inStructPath.through match {
                        case listType: TList[_] => //pathGet(path) ~ pathPut(path) ~ listPost(path) ~ pathDelete(path)
                          directAlterPut(opt, doc, path) ~ directAlterDelete(opt, doc, path) ~ post {
                            hasEntity {
                              case true =>
                                val listPath = ListPath(path.inStructPath, -1)
                                mapRequest(_.copy(method = RestMethods.PUT)) {
                                  directAlterPut(opt, doc, listPath)
                                }
                              case false =>
                                newAlterSession(opt, doc, path) ~ directAlterDelete(opt, doc, path)
                            }
                          }
                        case _ => newAlterSession(opt, doc, path) ~ directAlterPut(opt, doc, path) ~ directAlterDelete(opt, doc, path)
                      }
                  }
              }
          }
      }

    def newAlterSession(opt: AuthorizedOperator, doc: T#Pack, path: StructPath): Route = post { ctx =>
      val seq = nextSeq
      val newSession = AlterSession(seq, opt, VersionRevise(doc.version, doc.revise), path.toString)
      context.actorOf(Props(new AlterActor(path, doc, newSession)), seq.toString)
      hlog(opt)(log.info("alter session {} created,{}", seq, path))
      alterSessions(seq) = newSession
      context.parent ! AlterSessionsChanged(alterSessions.values.toList, None, None, Nil)
      ctx.complete(ActionResult(seq, "", VersionRevise(doc.version, doc.revise)))
    }

    //执行直接修改的 put 动作
    def directAlterPut(opt: AuthorizedOperator, doc: T#Pack, path: StructPath): Route = put {
      parameterMap { params =>
        val merge = params.get("merge")
        val action = params.get("action")
        entity(unpack(path.targetType)) {
          pk => onComplete(execDirectAlter(doc, Some(pk), opt, path, merge, action, params)) {
            case Success(res) =>
              ctx =>
                if (res._1 == OK) hlog(opt)(log.info("direct alter success,{} = {}", path, pk))
                ctx.complete(res._1, res._2)
            case Failure(e: Throwable) =>
              ctx =>
                hlog(opt)(log.error("direct alter data failed,{}. reason:{}", path, e.stackTrace))
                ctx.complete(InternalServerError, ActionResult(docState, e.getMessage, VersionRevise(doc.version, doc.revise)))
          }
        }
      }
    }

    //执行直接修改的 delete 动作
    def directAlterDelete(opt: AuthorizedOperator, doc: T#Pack, path: StructPath): Route = delete {
      //parameters("action".as[String] ?) { action =>
      parameterMap { params =>
        val action = params.get("action")
        onComplete(execDirectAlter(doc, None, opt, path, Some(MergeManner.Replace.toString), action, params)) {
          case Success(res) => complete(res._1, res._2)
          case Failure(e: RequiredFieldNotSetException) =>
            complete(BadRequest, ActionResult(docState, e.getMessage, VersionRevise(doc.version, doc.revise)))
          case Failure(e:AlterDataFailed) =>
            ctx =>
              hlog(opt)(log.warning("direct delete data failed,{}. reason:{}", path, e.cause.stackTrace))
              ctx.complete(BadRequest, ActionResult(docState, e.cause.getMessage, VersionRevise(doc.version, doc.revise)))
          case Failure(e: Throwable) =>
            ctx =>
              hlog(opt)(log.warning("direct delete data failed,{}. reason:{}", path, e.stackTrace))
              ctx.complete(InternalServerError, ActionResult(docState, e.getMessage, VersionRevise(doc.version, doc.revise)))
        }
      }
    }


    //不创建 AlterActor 直接修改数据,在这一个函数中完成变更会话的创建、执行、提交/放弃
    def execDirectAlter(orientDoc: T#Pack, data: Option[ValuePack[Any]], opt: AuthorizedOperator, path: StructPath,
                        merge: Option[String], action: Option[String], params: Map[String, String]): Future[(StatusCode, ActionResult)] = {

      val promise = Promise[(StatusCode, ActionResult)]()
      if (path.inStructPath.length == 1 && data.isEmpty) {
        promise.success((Forbidden, ActionResult(docState, "can't delete total document", VersionRevise(orientDoc.version, orientDoc.revise))))
      } else if (action.isDefined && !supportAction(opt, action.get, withEntity = false)) {
        val msg = s"${if (opt.isManager) "manager" else "user"} can't execute action ${action.get}"
        hlog(opt)(log.warning(msg))
        promise.success((Forbidden, ActionResult(docState, msg, VersionRevise(orientDoc.version, orientDoc.revise))))
      } else {
        //校验能否进行数据修改
        alterVerify(orientDoc, path, data, opt) match {
          //可以修改数据
          case Right(goodPk) =>
            //创建变更会话
            val seq = nextSeq
            val newSession = AlterSession(seq, opt, VersionRevise(orientDoc.version, orientDoc.revise), path.toString)
            //context.actorOf(Props(new AlterActor(path, doc, newSession)), seq.toString)
            alterSessions(seq) = newSession
            context.parent ! AlterSessionsChanged(alterSessions.values.toList, None, None, Nil)

            try {
              //执行数据修改
              val alteredDoc = path.alterDomainData(orientDoc, goodPk, MergeManner.fromString(merge))
              //提交数据修改
              submitAlter(alteredDoc, opt, seq, action, params).onComplete {
                case Success(res) =>
                  self ! AlterOver(seq, orientDoc, Some(alteredDoc), List(path))
                  promise.success(res)
                case Failure(e: Throwable) =>
                  self ! AlterOver(seq, orientDoc)
                  promise.failure(e)
              }
            } catch {
              case e: Throwable =>
                self ! AlterOver(seq, orientDoc)
                promise.failure(AlterDataFailed("",e))
            }

          //无权修改数据
          case Left((statusCode, errMsg)) =>
            hlog(opt)(log.info("refuse direct alter {}", path.toString))
            promise.success((statusCode, ActionResult(docState, errMsg, VersionRevise(orientDoc.version, orientDoc.revise))))
        }
      }
      promise.future
    }

    case object GetAlterSessions

    class AlterActor(val allowPath: StructPath, val orientDoc: T#Pack, alter: AlterSession) extends RestServiceActor with AuthorizeDirectives with DomainDirectives {
      def receive: Receive = innerReceive orElse runRoute(route)

      var alteredDoc = orientDoc

      //变更列表，只记路径，后修改的路径在头部
      var changeList = List[StructPath]()

      def innerReceive: Receive = {
        case AlterHalt =>
          context.parent ! AlterOver(alter.seq, orientDoc)
          self ! PoisonPill
      }

      def route: Route =
        operatorIs(alter.opt.roles.keySet) {
          handDoc(domainType, domainRoute)
        }

      //获取指定路径的数据
      private def pathGet(path: StructPath): Route = get {
        ctx =>
          path.getDomainData(alteredDoc) match {
            case Some(v) => ctx.complete(v)
            case None => ctx.complete(NotFound)
          }
      }

      //修改指定路径的数据，pathIn 用来检查要修改的路径是否在允许的范围（创建变更会话时指定的）之内
      private def pathAlter(path: StructPath): Route = {
        def checkAbandon(abandon: Boolean, opt: AuthorizedOperator) {
          if (abandon) {
            hlog(opt)(log.warning("alter session {} force abandon for alter error", alter.seq))
            context.parent ! AlterOver(alter.seq, orientDoc)
            self ! PoisonPill
          }
        }
        pathIn(allowPath, path) {
          operator {
            opt =>
              parameters("merge".as[String] ?, 'errorThenAbandon.as[Boolean] ?) {
                (merge, errorThenAbandon) =>
                  entity(unpack(path.targetType, () => {
                    errorThenAbandon.asInstanceOf[Option[Boolean]].map(checkAbandon(_, opt))
                  })) {
                    pk => ctx =>
                      alterVerify(alteredDoc, path, Some(pk), opt) match {
                        case Right(goodPk) =>
                          try {
                            alteredDoc = path.alterDomainData(alteredDoc, goodPk, MergeManner.fromString(merge))
                            changeList = path :: changeList
                            hlog(opt)(log.info("alter session {} set data success,{} = {}", alter.seq, path, goodPk.map(_.toJsValue).getOrElse("")))
                            ctx.complete(ActionResult(0, "", VersionRevise(alteredDoc.version, alteredDoc.revise)))
                          } catch {
                            case e: Throwable =>
                              hlog(opt)(log.warning("alter session {} set data failed,{} = {}", alter.seq, path, goodPk.map(_.toJsValue).getOrElse("")))
                              val msg = e.getMessage
                              errorThenAbandon.asInstanceOf[Option[Boolean]].map(checkAbandon(_, opt))
                              ctx.complete(BadRequest, msg)
                          }

                        case Left((statusCode, errMsg)) =>
                          errorThenAbandon.asInstanceOf[Option[Boolean]].map(checkAbandon(_, opt))
                          ctx.complete(statusCode, errMsg)
                      }
                  }
              }
          }
        }
      }

      private def pathDelete(path: StructPath): Route = {
        def checkAbandon(abandon: Boolean, opt: AuthorizedOperator) {
          if (abandon) {
            hlog(opt)(log.warning("alter session {} force abandon for alter error", alter.seq))
            context.parent ! AlterOver(alter.seq, orientDoc)
            self ! PoisonPill
          }
        }
        delete {
          pathIn(allowPath, path) {
            operator {
              opt =>
                parameters("errorThenAbandon".as[Boolean] ?) { errorThenAbandon =>
                  ctx =>
                    alterVerify(alteredDoc, path, None, opt) match {
                      case Right(goodPk) =>
                        try {
                          alteredDoc = path.alterDomainData(alteredDoc, goodPk, MergeManner.Replace)
                          changeList = path :: changeList
                          ctx.complete(ActionResult(0, "", VersionRevise(alteredDoc.version, alteredDoc.revise)))
                        } catch {
                          case e: Throwable =>
                            hlog(opt)(log.warning("alter session {} delete data failed,{}", alter.seq, path))
                            errorThenAbandon.map(checkAbandon(_, opt))
                            ctx.complete(BadRequest, e.getMessage)
                        }
                      case Left((statusCode, errMsg)) =>
                        ctx.complete(statusCode, errMsg)
                    }
                }
            }
          }
        }
      }

      // Put方法修改指定路径的数据
      private def pathPut(path: StructPath): Route = put {
        pathAlter(path)
      }

      //Post方法向列表末尾添加数据，转换成与 Put 方法相同的处理模式，但是在路径上把列表的索引值设置为-1
      private def listPost(path: StructPath): Route = post {
        pathAlter(ListPath(path.inStructPath, -1))
      }


      //提交变更会话
      private def alterSubmit: Route = post {
        operator {
          opt =>
            // 检查有没有同时提交的动作请求
            parameterMap { params =>
              val action: Option[String] = params.get("action")
              if (action.isDefined && !supportAction(opt, action.get, withEntity = false)) {
                val msg = s"${if (opt.isManager) "manager" else "user"} can't execute action ${action.get}"
                hlog(opt)(log.warning("alter session {} submit failed,force abandon {}！ reason:{}", alter.seq, alter.path, msg))
                context.parent ! AlterOver(alter.seq, orientDoc)
                complete(Forbidden, ActionResult(docState, msg, VersionRevise(orientDoc.version, orientDoc.revise)))
              } else {
                onComplete(submitAlter(alteredDoc, opt, alter.seq, action, params)) {
                  //结束修改，提交文档
                  case Success(res) =>
                    ctx =>
                      hlog(opt)(log.info("alter session {} submited,{}", alter.seq, alter.path))
                      context.parent ! AlterOver(alter.seq, orientDoc, Some(alteredDoc), changeList)
                      self ! PoisonPill
                      ctx.complete(res._1, res._2)
                  case Failure(e: Throwable) =>
                    ctx =>
                      hlog(opt)(log.error("alter session {} submit failed,force abandon {}！ reason:{}", alter.seq, alter.path, e.stackTrace))
                      context.parent ! AlterOver(alter.seq, orientDoc)
                      complete(InternalServerError, ActionResult(docState, e.getMessage, VersionRevise(alteredDoc.version, alteredDoc.revise)))
                }
              }
            }
        }
      }

      //放弃变更会话
      private def alterAbandon: Route = delete {
        operator {
          opt =>
            ctx =>
              hlog(opt)(log.info("alter session {} abandon", alter.seq))
              context.parent ! AlterOver(alter.seq, orientDoc)
              self ! PoisonPill
              ctx.complete(ActionResult(docState, "", VersionRevise(orientDoc.version, orientDoc.revise)))
        }
      }

      private def domainRoute(path: StructPath): Route =
        if (path.inStructPath.size == 1) {
          pathGet(path) ~ pathPut(path) ~ alterSubmit ~ alterAbandon
        } else {
          path.inStructPath.through match {
            case listType: TList[_] => pathGet(path) ~ pathPut(path) ~ listPost(path) ~ pathDelete(path)
            case _ => pathGet(path) ~ pathPut(path) ~ pathDelete(path)
          }
        }
    }

  }

}

object DomainActor {

  val sysopt = AuthorizedOperator("system", isManager = true)

  case object StartInit

  case class InitDocOver[T <: TStruct](result: Either[Throwable, T#Pack])

  implicit class WaitInit(domainActor: ActorRef) {
    def startInit[T <: TStruct](implicit timeout: Timeout): Future[InitDocOver[T]] = {
      (domainActor ? StartInit).asInstanceOf[Future[InitDocOver[T]]]
    }
  }

  implicit def checkSuccess[T <: TStruct](future: ⇒ Future[InitDocOver[T]])(implicit hl: HListable[Either[Throwable, T#Pack]], ec: ExecutionContext) =
    new Directive[hl.Out] with OnSuccessFutureMagnet {
      type Out = hl.Out

      def get = this

      def happly(f: Out ⇒ Route) = ctx ⇒ future.onComplete {
        case Success(t) ⇒
          try f(hl(t.result))(ctx)
          catch {
            case NonFatal(error) ⇒ ctx.failWith(error)
          }
        case Failure(error) ⇒ ctx.failWith(error)
      }
    }

  implicit def checkComplete[T <: TStruct](future: ⇒ Future[InitDocOver[T]])(implicit ec: ExecutionContext) =
    new OnCompleteFutureMagnet[InitDocOver[T]] {
      def happly(f: (Try[InitDocOver[T]] :: HNil) ⇒ Route): (RestReqContext) ⇒ Unit = ctx ⇒
        try future.onComplete(t ⇒ f(t :: HNil)(ctx))
        catch {
          case NonFatal(error) ⇒ ctx.failWith(error)
        }
    }
}

case class VersionRevise(version: Int, revise: Int)

class AlterTerminatorActor(alters: immutable.Iterable[ActorRef], reportTo: ActorRef) extends Actor {
  private var count = 0

  val ss = alters.size

  alters.foreach { a =>
    count = count + 1
    context.watch(a)
  }

  def receive = {
    case Terminated(a) =>
      count = count - 1
      if (count == 0) {
        reportTo ! AlterAbandoned
        self ! PoisonPill
      }
  }
}

object VersionRevise extends DefaultJsonProtocol {
  implicit val format = jsonFormat2(VersionRevise.apply)
}

case object AlterAbandoned

case class AlterSession(seq: Int, opt: AuthorizedOperator, initVer: VersionRevise, path: String)

object AlterSession extends DefaultJsonProtocol {
  implicit val format = jsonFormat4(AlterSession.apply)
}

case class ResourceNotFound(id: String) extends Exception(s"request resource $id not found!")

case class RestException(err: StatusCode) extends Exception(err.value)

case class AlterDataFailed(message:String,cause:Throwable) extends Exception(message,cause)

private object DocLoadStatus extends Enumeration {
  val DocUnload = Value(1, "Unload")
  val DocLoading = Value(2, "Loading")
  val DocLoaded = Value(3, "Loaded")
  val DocNotFound = Value(4, "NotFound")
  val DocLoadFailed = Value(5, "Failed")
}