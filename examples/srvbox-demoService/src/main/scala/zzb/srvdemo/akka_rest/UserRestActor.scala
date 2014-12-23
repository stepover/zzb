package zzb.srvdemo.akka_rest

import zzb.rest._
import akka.actor._
import zzb.srvdemo.entites._
import zzb.srvdemo.DBOperate
import akka.util.Timeout
import scala.concurrent.duration._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-6
 * Time: 上午10:49
 * Copyright baoxian.com 2012~2020
 */
class UserRestActor extends RestServiceActor {

  implicit val timeout = Timeout(10 seconds)


  def receive: Actor.Receive = runRoute(restRoute)


  def restRoute: zzb.rest.Route = {
    import zzb.rest._

    path("hello") {
      get {
        ctx ⇒
          ctx.complete("ok")
      }
    } ~
      path("user") {
        post {
          //新增
          entity(as[User]) {
            user ⇒
              complete(DBOperate.addUser(user))
          }
        } ~
          get {
            complete(DBOperate.listUsers())
          }
      } ~
      path("user" / IntNumber) {
        userId =>
          delete {
            complete(ChangeCount(DBOperate.delUser(userId)))
          } ~
            get {
                DBOperate.getUser(userId) match {
                  case Some(u) => complete(u)
                  case None => reject
                }
            } ~
            put {
              entity(as[User]) {
                user =>
                  if (userId != user.id)
                    reject(MalformedFormFieldRejection("id", "错误的user id"))
                  else
                    complete(DBOperate.updateUser(user))

              }
            }
      }
  }
}


