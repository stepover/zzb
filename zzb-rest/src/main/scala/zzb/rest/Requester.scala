package zzb.rest

import akka.actor._
import akka.util.Timeout
import spray.http.Uri
import spray.http.Uri.Path
import zzb.util.MdcLoggingContext
import zzb.util.log.{ HeritLogAdapter, HeritLog }

import scala.concurrent.{ Future, Promise }
import scala.util.{ Failure, Success }

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-9
 * Time: 上午11:25
 * Copyright baoxian.com 2012~2020
 */
trait Requester extends RequestBuilding {

  implicit class WithTransformation2(request: RestRequest) {
    def ~>[A, B](f: A ⇒ B)(implicit ta: TildeArrow[A, B]): ta.Out = ta(request, f)
  }

  abstract class TildeArrow[A, B] {
    type Out

    def apply(request: RestRequest, f: A ⇒ B): Out
  }

  object TildeArrow {

    implicit object InjectIntoRequestTransformer extends TildeArrow[RestRequest, RestRequest] {
      type Out = RestRequest

      def apply(request: RestRequest, f: RestRequest ⇒ RestRequest) = f(request)
    }

    implicit object DoRequestTransformer extends TildeArrow[RestRequest, Future[RestResponse]] {
      type Out = Future[RestResponse]

      def apply(request: RestRequest, f: RestRequest ⇒ Future[RestResponse]) = f(request)
    }

  }

  def doRequest(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): RestRequest ⇒ Future[RestResponse] = exeRequest

  private val hlog = HeritLog("rest")

  def exeRequest(req: RestRequest)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] = {
    val p = Promise[RestResponse]()
    futureNavigateTo(req.uri, req, p)
    p.future
  }

  def exeRequest(req: RestRequest, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = {
    actorNavigateTo(req.uri, req, responder)
  }

  private def actorNavigateTo(uri: Uri, req: RestRequest, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = {
    import spray.http.Uri.Path._
    import system.dispatcher

    val path = uri.path

    //implicit val timeout = Timeout(200 millis)
    path match {
      case Empty ⇒
        responder ! RestResponse(StatusCodes.NotFound, "Postman can't found Empty path actor")
      case SingleSlash ⇒
        responder ! RestResponse(StatusCodes.NotFound, "Postman can't found SingleSlash path actor")
      case Path(str) ⇒

        val baseUri = if (uri.isRelative) system.deadLetters.path.root
        else RootActorPath(Address(uri.scheme, uri.authority.userinfo, uri.authority.host.address, uri.authority.port))

        val actorPath: ActorPath = baseUri / ("user" :: str.split("/").toList)

        val actorSelection = system.actorSelection(actorPath)
        actorSelection.resolveOne().onComplete {
          case Success(actorRef) ⇒
            val unmatchedPath = req.uri.path.toString().replaceFirst(str, "")
            val ctx = RestReqContext(req, responder, Uri.Path(unmatchedPath))
            actorRef ! ctx
          /*
            val unmatchedPath = req.uri.path.toString().replaceFirst(str, "")
            actorRef.ask((req, unmatchedPath)).onComplete {
              case Success(response: RestResponse) ⇒
                if (log.isDebugEnabled) //每个请求都有日志输出，太狠了，所以前置挡一下
                  hlog(log.debug("{} => {}", req, response))
                p.success(response)
              case Success(r) ⇒
                hlog(log.error("rest request get response is not 'RestResponse',{} => {}", req, r))
                p.failure(WrongResponseTypeException(r))

              case Failure(e: AskTimeoutException) ⇒
                val ex = RestTimeoutException(req.uri.toString(), timeout.duration.toMillis)
                hlog(log.error("{}", ex))
                p.failure(ex)
              case Failure(e) ⇒
                hlog(log.error(e, "rest request error,{} => {}", req, e))
                p.failure(e)
            }
            */
          case Failure(ActorNotFound(s)) ⇒
            actorNavigateTo(parentPath(uri), req, responder)
          case Failure(e) ⇒
            responder ! RestResponse(StatusCodes.InternalServerError, e.getMessage)
        }
    }
  }

  /**
   * 根据给定的路径查找对应的Actor,将请求消息发送给他，如果没有找到对应的Actor
   * 就查找父级路径对应的Actor,如果最终还是找不到，就回应404错误
   * @param uri  指定的路径
   * @param req   RestRequest
   */
  private def futureNavigateTo(uri: Uri, req: RestRequest, p: Promise[RestResponse])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = {
    import spray.http.Uri.Path._
    import system.dispatcher
    import akka.pattern._

    val path = uri.path

    //implicit val timeout = Timeout(200 millis)
    path match {
      case Empty ⇒
        p.success(RestResponse(StatusCodes.NotFound, "Postman can't found Empty path actor"))
      case SingleSlash ⇒
        p.success(RestResponse(StatusCodes.NotFound, "Postman can't found SingleSlash path actor"))
      case Path(str) ⇒

        val baseUri = if (uri.isRelative) system.deadLetters.path.root
        else RootActorPath(Address(uri.scheme, uri.authority.userinfo, uri.authority.host.address, uri.authority.port))

        val actorPath: ActorPath = baseUri / ("user" :: str.split("/").toList)

        val actorSelection = system.actorSelection(actorPath)
        actorSelection.resolveOne().onComplete {
          case Success(actorRef) ⇒
            val unmatchedPath = req.uri.path.toString().replaceFirst(str, "")
            val reqActor = system.actorOf(Props(new PromiseResponseActor(req, p, timeout)))
            val ctx = RestReqContext(req, reqActor, Uri.Path(unmatchedPath))
            actorRef ! ctx
          case Failure(ActorNotFound(s)) ⇒
            futureNavigateTo(parentPath(uri), req, p)
          case Failure(e) ⇒
            p.success(RestResponse(StatusCodes.InternalServerError, e.getMessage))
        }
    }
  }

  /**
   * 获取一个uri的父级uri
   * @param uri  给定的路径
   * @return      父级路径
   */
  private def parentPath(uri: Uri): Uri = {
    import spray.http.Uri.Path._
    val path = uri.path
    path.reverse match {
      case Segment(head, Slash(tail))        ⇒ uri.withPath(tail.reverse)
      case Slash(Segment(head, Slash(tail))) ⇒ uri.withPath(tail.reverse)
      case _                                 ⇒ uri.withPath(Empty)

    }
  }
}

object Requester extends Requester

case object RequesterTimeout

class PromiseResponseActor(req: RestRequest, promise: Promise[RestResponse], to: Timeout)(implicit mdcLog: MdcLoggingContext) extends ResponderBase {

  import context.dispatcher

  context.system.scheduler.scheduleOnce(to.duration, self, RequesterTimeout)

  override def requestId = req.toString

  override def timeout: Option[Timeout] = Some(to)

  override def onTimeout() = {
    if (!promise.isCompleted) {
      super.onTimeout()
      promise.failure(RestTimeoutException(req.uri.toString(), to.duration.toMillis))
    }
  }

  override def onFailed(e: Throwable) = promise.failure(e)

  override def onResponse(): Unit = {
    promise.success(response.get)
    super.onResponse()
  }

  override def log = HeritLogAdapter("rest", mdcLog)
}

case class WrongResponseTypeException(data: Any) extends Exception

case class RestTimeoutException(uri: String, millis: Long) extends Exception(s"rest request to $uri timeout. after $millis ms")
