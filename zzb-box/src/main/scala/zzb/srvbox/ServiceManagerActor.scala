package zzb.srvbox

import spray.routing._
import akka.actor._
import zzb.srvbox.SrvManageProtocol._
import spray.http.StatusCodes
import akka.util.Timeout
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import spray.httpx.SprayJsonSupport._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-16
 * Time: 下午3:44
 * Copyright baoxian.com 2012~2020
 */
class ServiceManagerActor extends HttpServiceActor with ServiceManagerApi {

  def receive = {
    case RestRequest("sys", ctx) ⇒
      routes(ctx)
    case restRequest: RestRequest ⇒
      boxActor ! restRequest
  }

}

trait ServiceManagerApi extends HttpService with ActorLogging {
  actor: Actor ⇒

  implicit val timeout = Timeout(10 seconds)

  import akka.pattern.ask
  import akka.pattern.pipe
  import context.dispatcher
  import scala.collection.JavaConversions._

  val boxActor = context.system.actorSelection("/user/boxActor")
  val managerHost:List[String] =
    if (context.system.settings.config.hasPath("http.managerHost"))
      context.system.settings.config.getStringList("http.managerHost").toList
    else
      List("localhost", "127.0.0.1")

  def routes: Route =

    path("list") {
      get {
        requestContext ⇒
          val responder = createResponder(requestContext)
          boxActor.ask(RequestList).pipeTo(responder)
      }
    } ~
      path("start") {
        post {
          entity(as[RequestStart]) {
            requestStart ⇒
              requestContext ⇒
                val responder = createResponder(requestContext)
                boxActor.ask(requestStart).pipeTo(responder)
          }
        }
      } ~
      path("stop") {
        post {
          entity(as[RequestStop]) {
            requestStop ⇒
              requestContext ⇒
                val responder = createResponder(requestContext)
                boxActor.ask(requestStop).pipeTo(responder)
          }
        }
      } ~
      path("halt") {
        host(managerHost : _*) {
          ctx ⇒ {
            boxActor ! Halt
            ctx.complete(StatusCodes.OK)
          }
        }
      }

  def createResponder(requestContext: RequestContext) = context.actorOf(Props(new Responder(requestContext, boxActor)))

}

class Responder(requestContext: RequestContext, boxActor: ActorSelection) extends Actor with ActorLogging {

  import zzb.srvbox.SrvManageProtocol._
  import spray.httpx.SprayJsonSupport._

  def receive = {

    case status: ServiceStatus ⇒
      requestContext.complete(StatusCodes.OK, status)
      self ! PoisonPill

    case Services(services) ⇒
      requestContext.complete(StatusCodes.OK, services)
      self ! PoisonPill

    case ServiceNotExist ⇒
      requestContext.complete(StatusCodes.NotFound)
      self ! PoisonPill
  }
}