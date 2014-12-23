package zzb.domain

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.util.Timeout
import spray.http.StatusCodes._
import zzb.datatype.DataType
import zzb.domain.directive.{AuthorizeDirectives, AuthorizedOperator}
import zzb.rest._
import zzb.storage.TStorable

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
 * 带数据创建Domain的方法
 * <p/>
 * 你的DomainSetActor继承该类，实现以下几个方法（以Plane为例子） ：<br/>
 * val docType: Plane.type = Plane
 * <br/>
 * def initOKRoute(doc: Plane.Pack): StandardRoute = complete(TString(doc.id))       //初始化成功后响应id
 * <br/>
 * def createNewChildActor(initDocValue: Option[Plane.Pack], isNewCreate: Boolean)       //使用post请求的实体创建一个新的DomainActor；id可以是生成器生成的，也可以是实体文档中的id属性（必须标注为isRequired）
 * <br/>
 * def loadChildActorById(id: String, isNewCreate: Boolean)         // 根据请求的id加载doc从而创建一个DomainActor；可以从数据库判断是否存在该id
 * <br/>
 */
trait WithDocCreatedDomainSetActor[K, KT <: DataType[K], T <: TStorable[K, KT]] extends RestServiceActor with AuthorizeDirectives with DomainLogging {

  private val hlog = HeritLog("cdst")

  val docType: T

  override def receive: Receive = runRoute(route)

  def initOKRoute(doc: T#Pack): StandardRoute

  def route: Route =
    operator {
      opt =>
        pathEndOrSingleSlash {
          post {
            entity(unpack(docType)) {
              initDocValue =>
                createNewDomainRoute(opt, Some(initDocValue.asInstanceOf[T#Pack]), initOKRoute)
            }
          }
        } ~
          forwardChildFuture(childByName, context)
    }

  def createNewChildActor(initDocValue: Option[T#Pack], isNewCreate: Boolean): Future[Either[(StatusCode, String), ActorRef]]

  def loadChildActorById(id: String, isNewCreate: Boolean): Future[Either[(StatusCode, String), ActorRef]]

  def childByName(id: String): Future[Either[(StatusCode, String), ActorRef]] = {
    import zzb.domain.DomainActor._
    val p = Promise[Either[(StatusCode, String), ActorRef]]()

    //todo:处理 DomainSetActor 创建子actor 超时
    implicit val timeout = Timeout(2000.millis)
    import context.dispatcher

    context.child(id) match {
      case Some(subAct) =>
        hlog(log.debug("found child '{}'", id))
        p.success(Right(subAct))
      case None =>
        hlog(log.debug("start load  child '{}' ...", id))
        try {
          loadChildActorById(id, isNewCreate = false) onComplete {
            case scala.util.Success(res) => res match {
              case Left((statusCode, msg)) => hlog(log.error("create NewDomain failed----{},{}", statusCode, msg))
                p.success(Left(statusCode, msg))
              case Right(subAct) => subAct.startInit[T].onComplete {
                case scala.util.Success(InitDocOver(Right(doc))) => p.success(Right(subAct))
                case scala.util.Success(InitDocOver(Left(e@RestException(_)))) =>
                  p.success(Left(e.err, e.err.reason))
                case scala.util.Success(InitDocOver(Left(e))) =>
                  hlog(log.error(e, "load NewDomain failed 1."))
                  p.success(Left(InternalServerError, e.getMessage))
                case scala.util.Failure(e) =>
                  hlog(log.error(e, "load NewDomain failed 2."))
                  p.success(Left(InternalServerError, e.getMessage))
              }
            }
            case scala.util.Failure(e) =>
              hlog(log.error(e, "load NewDomain failed 3."))
              p.success(Left(InternalServerError, e.getMessage))
          }
        } catch {
          case e: Throwable =>
            hlog(log.error(e, "load NewDomain failed 3."))
            p.success(Left(InternalServerError, e.getMessage))
        }

    }
    p.future
  }

  def createNewDomainRoute(opt: AuthorizedOperator, initDocValue: Option[T#Pack], createDocOverRoute: T#Pack => Route): Route = {
    import context.dispatcher
    import zzb.domain.DomainActor._
    try {
      implicit val timeout = Timeout(5, TimeUnit.SECONDS)
      hlog(log.debug("creating new domain Actor ..."))
      onSuccess(createNewChildActor(initDocValue, isNewCreate = true)){
        case Left((statusCode, msg)) => hlog(log.error("create NewDomain failed----{},{}", statusCode, msg))
          complete(statusCode, msg)
        case Right(theActor) =>
          onSuccess(checkSuccess(theActor.startInit[T])) {
            case Right(doc) =>
              createDocOverRoute(doc)
            case Left(e@RestException(_)) =>
              complete(e.err, e.err.reason)
            case Left(e) =>
              hlog(log.error(e, "create NewDomain failed."))
              complete(InternalServerError, e.getMessage)
          }
      }
    } catch {
      case e: Throwable =>
        hlog(log.error(e, "create NewDomain failed "))
        complete(InternalServerError, e.getMessage)
    }
  }

}
