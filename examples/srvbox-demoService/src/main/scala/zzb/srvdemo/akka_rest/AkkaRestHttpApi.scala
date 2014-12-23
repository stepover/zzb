package zzb.srvdemo.akka_rest

import spray.routing.{HttpServiceActor, Route}
import zzb.rest.http2akka.Http2AkkaDirectives
import akka.util.Timeout
import scala.concurrent.duration._
import zzb.srvdemo.entites._
import spray.httpx.SprayJsonSupport._


/**
 * Created by Simon on 2014/3/23.
 */
class AkkaRestHttpApi extends HttpServiceActor with Http2AkkaDirectives {
  override def receive: Receive = runRoute(routes)

  implicit val timeout = Timeout(10 seconds)

  implicit val system = context.system

  def routes: Route =
    path("hello") {
      get {
        akkaCompleteAs[String]
      }
    } ~ path("user") {
      post {
        //新增
        entity(as[User]) {
          user ⇒
            akkaWithEntityCompleteAs[User](user)
        }
      } ~
        get {
          akkaCompleteAs[List[User]]
        }
    } ~
      path("user" / IntNumber) {
        userId =>
          delete {
            akkaCompleteAs[ChangeCount]
          } ~
            get {
              akkaCompleteAs[User]
            } ~
            put {
              entity(as[User]) {
                user =>
                  akkaWithEntityCompleteAs[User](user)
              }
            }
      }


}
