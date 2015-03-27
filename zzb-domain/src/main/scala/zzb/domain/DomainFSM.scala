package zzb.domain

import akka.actor.{ActorRef, FSM, PoisonPill}
import akka.pattern._
import akka.util.Timeout
import shapeless.{HNil, ::}
import spray.http.StatusCodes._
import zzb.datatype.BasicFormats._
import spray.json.JsonFormat
import zzb.datatype._
import zzb.domain.directive.AuthorizedOperator
import zzb.rest._
import zzb.rest.directives.{OnCompleteFutureMagnet, OnSuccessFutureMagnet}
import zzb.util._
import zzb.storage.TStorable
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success}

/**
 * Created by Simon on 2014/5/13
 */
trait DomainFSM[K, KT <: DataType[K], T <: TStorable[K, KT], S <: Enumeration#Value]
  extends DomainActor[K, KT, T] with FSM[S, T#Pack] {


  private val hlog = HeritLog("fsm")

  implicit val domainSystem = context.system

  implicit def docExecutionContext = context.dispatcher

  def executingState: S

  implicit def takeDocState(doc: T#Pack): S

  override def receive: Receive = domainFsmReceive orElse super[DomainActor].msgReceive orElse super[FSM].receive

  protected def domainFsmReceive: Receive = {
    case GetFSMData => sender() ! stateData
  }

  import DomainActor._

  private var fsmStarted = false

  protected def initStateFromDoc(doc: T#Pack) = takeDocState(doc)

  override def initDocOver(res: InitDocOver[T]) {
    res match {
      case m@InitDocOver(Right(doc)) =>
        mdc += ("own" -> getOwnerFromDoc(doc))
        val initState = initStateFromDoc(doc)
        startWith(initState, doc)
        fsmStarted = true
        hlog(sysopt)(log.info(s"fsm started on state = {}", initState))
      case m@InitDocOver(Left(e)) =>
        hlog(sysopt)(log.info(s"fsm started failed because init failed"))
      case _ => ()
    }
  }

  override def postStop(): Unit = {
    if (fsmStarted)
      super.postStop() //如果状态机启动失败，就不能调用 FSM 的postStop,因为里面的 stay 会导致空指针异常
    else
      super[DomainActor].postStop()

  }

  implicit val timeout = Timeout(5005 milliseconds)

  /**
   * 文档变更校验。DomainActor 在执行文档变更之前会先调用子类的这个方法，每一个具体的业务文档类型
   * 可以在这里执行变更前的校验逻辑，如果不允许修改则返回错误信息 Left(StatusCode, String)
   * 如果允许就返回指定路径的新值 Right(ValuePack[Any])
   * @param curDoc 待变更的文档内容
   * @param path 待变更的文档内部路径
   * @param fieldValue 指向文档路径的新值
   * @return 返回错误信息或，允许采用的变更值
   */
  override def alterVerify(curDoc: T#Pack,
                           path: StructPath,
                           fieldValue: Option[ValuePack[Any]],
                           opt: AuthorizedOperator): Either[(StatusCode, String), Option[ValuePack[Any]]] = {
    checkAlterBlock(stateName, opt, path.toString)(_.startsWith(_)) match {
      case Some(msg) =>
        hlog(opt)(log.info("refuse alter {},reason: {}", path, msg))
        Left((StatusCodes.Forbidden, msg))
      case None => Right(fieldValue)
    }
  }

  //implicit def optActionTrans(opt: AuthorizedOperator) = if (opt.isManager) Manager else User

  override def alterPathVerify(path: StructPath, operator: AuthorizedOperator): Option[String] =
    checkAlterBlock(stateName, operator, path.toString)((p1, p2) => p1.startsWith(p2) || p2.startsWith(p1))

  def docState: Int = stateName.id

  def when(stateNames: S*)(stateFunction: StateFunction): Unit =
    stateNames.foreach(s => when(s, null)(stateFunction))

  def blockAlter(blockRole: String, path: Path, msg: String, states: S*):Unit =
    blockAlter(Set(blockRole),path,msg,states:_*)

  def blockAlter(blockRoles: Set[String], path: Path, msg: String, states: S*): Unit =
    states.foreach(
      stateBlackList.getOrElseUpdate(_, mutable.Set[BlockRule]()).add(BlockRule(blockRoles.map(_.toLowerCase), path, msg))
    )

  def route: Route = domainRoute

  /**
   * 提交修改后的文档
   * @param doc 新文档
   * @param operator 操作者
   * @return
   */
  override def submitAlter(doc: T#Pack, operator: AuthorizedOperator, seq: Int,
                           action: Option[String], params: Map[String, String]): Future[(StatusCode, ActionResult)] = {

    if (action.isDefined)
      hlog(operator)(log.info("alter session {} submitting with action '{}' ...", seq, action.get))
    else
      hlog(operator)(log.info("alter session {} submitting... ", seq))

    val p = Promise[(StatusCode, ActionResult)]()

    val revise = DocRevise(List(DocNodeRevise(NodePath(doc.dataType.me.path), doc, MergeManner.Replace)), operator)


    val f1 = self ? ApplyRevise(revise, null,params.getOrElse("tag", "") ,1) //给自己发送事件，由状态机中的处理完成实际的存储
    val res_f = if (action.isDefined) {
        for {
          v1 <- f1
          v2 <- self ? Action(action.get, operator, params, None) //actionHandler(operator)((operator, action.get))
        } yield v2
      } else f1

    res_f.onComplete {
      case Success(v) => p.success(anyToActionResult(v))
      case Failure(e) => p.failure(e)
    }
    p.future
  }

  private val dataAccessRcv = runRoute(route)

  def restAccessHandler: StateFunction = {
    case Event(ctx: RestReqContext, _) =>
      dataAccessRcv(ctx)
      stay()
    case Event(req: RestRequest, _) =>
      dataAccessRcv(req)
      stay()
    case Event(c: (_, _), _) =>
      dataAccessRcv(c)
      stay()
  }

  when(executingState) {
    case Event(ExeSuccess(data, sender, jobDesc), _) =>
      if (sender != null && sender != context.system.deadLetters) sender ! data

      data match {
        case doc: T#Pack =>
          goto(takeDocState(doc)) using doc //修改状态机中的数据
        case Some(doc: T#Pack) =>
          goto(takeDocState(doc)) using doc //修改状态机中的数据
        case _ =>
          if (stateData != null) goto(takeDocState(stateData))
          else {
            self ! PoisonPill
            stop(FSM.Shutdown)
          }
      }

    case Event(ExeFailed(e, sender, jobDesc), _) =>
      if (sender != null && sender != context.system.deadLetters) sender ! e
      hlog(sysopt)(log.error("long time job {} execute failed,cause:{}", jobDesc, e.stackTrace))
      goto(takeDocState(stateData))
  }

  onTransition {
    case st@(_ -> _) =>
      hlog(sysopt)(log.info("transition state  {} -> {}", st._1, st._2))
  }

  def submitAlterHandler: StateFunction = {
    case Event(ApplyRevise(revise, replyTo,newTag, 1), doc) =>
      val newDoc = revise.nodeRevises.foldLeft(doc) { (d, n) => //应用每一项修改
        hlog(sysopt)(log.debug("ApplyRevise:{} = {}", n.path, n.value.json))
        n.path.alterDomainData(d, Some(n.value), n.merge)
      }
      doSave(newDoc, revise.opt, if (replyTo != null && replyTo != context.system.deadLetters) replyTo else sender(),newTag)
      stay()
    case Event(ApplyRevise(revise, replyTo,newTag, _), doc) =>
      val newDoc = revise.nodeRevises.foldLeft(doc) { (d, n) => //应用每一项修改
        hlog(sysopt)(log.debug("ApplyRevise:{} = {}", n.path, n.value.json))
        n.path.alterDomainData(d, Some(n.value), n.merge)
      }
      doSave(newDoc, revise.opt, if (replyTo != null && replyTo != context.system.deadLetters) replyTo else sender(),newTag)
      stay()
  }

  def longTimeExecHandler(): StateFunction = {
    case Event(LongTimeExec(longExec, doc, opt, jobDesc, replyTo), _) =>
      longExec(doc, opt).onComplete {
        case Success(v) =>
          self ! ExeSuccess(v, replyTo, jobDesc)
        case Failure(e) =>
          self ! ExeFailed(e, replyTo, jobDesc)
      }
      goto(executingState)
  }

  protected def doSave(newDoc: T#Pack, opt: AuthorizedOperator, replyTo: ActorRef = context.sender(),newTag :String = "") :Unit = {
//    if (onlyToMemoryCache) {
//      save(newDoc,opt.id, opt.isManager,onlyToMemoryCache)
//      if (replyTo != null && replyTo != context.system.deadLetters) replyTo ! newDoc
//    } else
    require(newTag ne null)
      self ! LongTimeExec((nd, op) => save(nd, op.id, op.isManager,newTag), newDoc, opt, "SaveDoc", replyTo)
  }

  protected override def anyToActionResult(any: Any): (StatusCode, ActionResult) = any match {
    case as: ActionResult =>
      (OK, as)
    case (sc: StatusCode, ar: ActionResult) =>
      (sc, ar)
    case (sc: StatusCode, ae: ActionError) =>
      (sc, ActionResult(ae.intValue, ae.reason, VersionRevise(stateData.version, stateData.revise)))
    case (sc: StatusCode, ar: String) =>
      (sc, ActionResult(stateName.id, ar, VersionRevise(stateData.version, stateData.revise)))
    case doc: T#Pack =>
      (OK, ActionResult(takeDocState(doc).id, "OK2", VersionRevise(doc.version, doc.revise)))
    case Some(doc: T#Pack) =>
      (OK, ActionResult(takeDocState(doc).id, "OK1", VersionRevise(doc.version, doc.revise)))
    case e: Throwable =>
      (InternalServerError, ActionResult(stateName.id, e.getMessage, VersionRevise(stateData.version, stateData.revise)))
    case _ =>
      (InternalServerError, ActionResult(stateName.id, "Failed", VersionRevise(stateData.version, stateData.revise)))
  }

  /**
   * 每个状态有个黑名单
   */
  private val stateBlackList = mutable.Map[S, mutable.Set[BlockRule]]()

  private def checkAlterBlock(state: S, operator: AuthorizedOperator, path: String)(pathChecker: (String, String) => Boolean) = {

    stateBlackList.get(state) match {
      case Some(list) =>
        list.find(i => operator.roles.keys.exists(i.blockRoles.contains) && pathChecker(path, i.path.value)) match {
          case Some(rule) => Some(rule.msg)
          case _ => None
        }
      case _ => None
    }
  }

  case class DelayedEvent(eventValue: AllowDelay, sender: ActorRef) {
    override def toString = eventValue.toString
  }

  var delayedEvents = List[DelayedEvent]()

  when(executingState) {
    case Event(cmd: AllowDelay, _) if stateName == executingState =>
      delayedEvents :+= DelayedEvent(cmd, sender())
      hlog(sysopt)(log.debug("delay cmd list added:{}", delayedEvents))
      stay()
  }

  whenUnhandled {
    case Event(cmd: Action, _) =>
      stay() replying ((Forbidden, s"${stateName.toString} can't do ${cmd.name}"))
    case a =>
      //hlog(log.warning("Unhandled event {}", a.toString))
      stay()

  }

  onTransition {
    case executingState -> _ =>
      hlog(sysopt)(log.debug("flush delay cmd :{}", delayedEvents))
      delayedEvents.foreach(e => self.tell(e.eventValue, e.sender))
      delayedEvents = Nil
  }

  implicit class UsingSaved(s : State) {
    def usingSaved(nextStateDate: T#Pack,opt:AuthorizedOperator, newTag:String = "",replyTo: ActorRef = context.sender()) = {
      doSave(nextStateDate,opt,replyTo,newTag)
      s using nextStateDate
    }

//    def usingCached(nextStateDate: T#Pack,opt:AuthorizedOperator, replyTo: ActorRef = context.sender()) = {
//      doSave(nextStateDate,opt,replyTo,onlyToMemoryCache = true)
//      s using nextStateDate
//    }

  }

  case class ExeSuccess(data: Any, sender: ActorRef, jobDesc: String = "")

  case class ExeFailed(exception: Throwable, sender: ActorRef, jobDesc: String)

  case class BlockRule(blockRoles: Set[String], path: Path, msg: String)

  case class LongTimeExec(longExec: (T#Pack, AuthorizedOperator) => Future[Any],
                          doc: T#Pack, opt: AuthorizedOperator = sysopt, jobDesc: String, replyTo: ActorRef = sender()) extends AllowDelay

  case class ApplyRevise(revise: DocRevise, replyTo: ActorRef = sender(),newTag:String = "" , mark: Int = 0) extends AllowDelay

}

case class DocNodeRevise(path: NodePath, value: ValuePack[Any], merge: MergeManner.Value = MergeManner.UseNew)

case class DocRevise(nodeRevises: List[DocNodeRevise], opt: AuthorizedOperator = DomainActor.sysopt)

case object GetFSMData




