package zzb.srvdemo.http_rest

import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor._
import zzb.srvdemo.SomeActor
import spray.http.StatusCodes
import spray.routing._
import zzb.srvdemo.entites._
import spray.httpx.SprayJsonSupport._


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-7
 * Time: 下午1:52
 * Copyright baoxian.com 2012~2020
 */
class UserHttpActor extends HttpServiceActor {

  implicit val timeout = Timeout(10 seconds)

  import zzb.srvdemo.DemoProtocol._
  import akka.pattern.ask
  import akka.pattern.pipe
  import context.dispatcher


  def receive = runRoute(routes)

  val someActor = actorRefFactory.actorOf(Props[SomeActor], "srvdemo.someActor")

  def routes =
    path("hello") {
      pathEndOrSingleSlash {
        get {
          ctx ⇒
            ctx.complete("ok")
          //          someActor.ask(ListUser).pipeTo(responder)
        }
      } ~
      path("good"){
        get {
          ctx ⇒
            ctx.complete("good")
          //          someActor.ask(ListUser).pipeTo(responder)
        }
      }
    } ~
      path("user") {
        post {
          //新增
          entity(as[User]) {
            user ⇒
              ctx ⇒
                val responder = createResponder(ctx)
                someActor.ask(AddUser(user)).pipeTo(responder)
          }
        } ~
          put {
            //修改
            entity(as[User]) {
              user ⇒
                ctx ⇒
                  val responder = createResponder(ctx)
                  someActor.ask(UpdateUser(user)).pipeTo(responder)
            }
          } ~
          get {
            ctx ⇒
              val responder = createResponder(ctx)
              someActor.ask(ListUser).pipeTo(responder)
          }
      } ~
      path("user" / IntNumber) {
        userId =>
          delete {
            ctx ⇒
              val responder = createResponder(ctx)
              someActor.ask(DelUser(userId)).pipeTo(responder)
          } ~
            get {
              ctx ⇒
                val responder = createResponder(ctx)
                someActor.ask(GetUser(userId)).pipeTo(responder)
            }
      }


  def createResponder(requestContext: RequestContext) = {
    actorRefFactory.actorOf(Props(new HttpResponder(requestContext, someActor)))
  }

}

class HttpResponder(requestContext: RequestContext, boxActor: ActorRef) extends Actor with ActorLogging {

  def receive = {
    case users: List[_] =>
      requestContext.complete(StatusCodes.OK, users.asInstanceOf[List[User]])
      self ! PoisonPill
    case user: User =>
      requestContext.complete(StatusCodes.OK, user)
      self ! PoisonPill
    case _ =>
      requestContext.complete(StatusCodes.OK, "true")
      self ! PoisonPill
  }
}
