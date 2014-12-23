package zzb.rest

import akka.actor.{ Actor, ActorSystem, Props }
import akka.util.Timeout
import org.scalatest.{ BeforeAndAfterAll, MustMatchers, WordSpec }
import spray.http.Uri
import spray.util._

import scala.concurrent.duration._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-10
 * Time: 下午5:00
 * Copyright baoxian.com 2012~2020
 */
class RestRouteTest extends WordSpec
  with MustMatchers
  with Caller with BeforeAndAfterAll {

  implicit val system = ActorSystem("route")

  override protected def beforeAll() {
    import zzb.rest.routing._

    system.actorOf(Props(new LooseActor("loose")), "loose")
    system.actorOf(Props(new StrictActor("strict")), "strict")
    system.actorOf(Props(new AutoActor("auto")), "auto")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(500 milliseconds)

  implicit val rootUri = Uri("")

  "resource actor with route " must {

    "can accept request by uri  " in {
      //这里的几个请求的URL都是存在的，会得到正确的反馈
      val looseRes = doGet("/loose").await
      looseRes.status must equal(StatusCodes.OK)
      looseRes.entity.data must equal("loose")

      val strictRes = doGet("/strict").await
      strictRes.status must equal(StatusCodes.OK)
      strictRes.entity.data must equal("strict")

      val autoRes = doGet("/auto").await
      autoRes.status must equal(StatusCodes.OK)
      autoRes.entity.data must equal("auto")
    }

    "if request uri not found,will be send to parent path " in {
      //请求的资源不存在，会自动发给父资源，LooseActor 通盘接收，正确响应
      val looseRes = (Get("/loose/nothis") ~> doRequest).await
      looseRes.status must equal(StatusCodes.OK)
      looseRes.entity.data must equal("loose")

      //请求的资源不存在，会自动发给父资源，StrictActor 回复 404 NotFound
      val strictRes = (Get("/strict/nothis") ~> doRequest).await
      strictRes.status must equal(StatusCodes.NotFound)

      //请求的资源不存在，会自动发给父资源，AutoActor 自动创建子资源并转发给它
      val autoRes1 = (Get("/auto/nothis") ~> doRequest).await
      autoRes1.status must equal(StatusCodes.OK)
      autoRes1.entity.data must equal("nothis")

      val autoRes2 = (Get("/auto/nothis/no_again") ~> doRequest).await
      autoRes2.status must equal(StatusCodes.OK)
      autoRes2.entity.data must equal("no_again")
    }

    "if request uri found,but create error will reject " in {

      //请求的资源不存在，会自动发给父资源，AutoActor 自动创建子资源并转发给它
      val autoRes1 = (Get("/auto/nothis") ~> doRequest).await
      autoRes1.status must equal(StatusCodes.OK)
      autoRes1.entity.data must equal("nothis")

      val autoRes2 = (Get("/auto/nothis/reject") ~> doRequest).await
      autoRes2.status must equal(StatusCodes.BadRequest)
      autoRes2.entity.data must equal("error")
    }
  }

}

package routing {

  import akka.actor.ActorRef

  class LooseActor(name: String) extends RestServiceActor {

    def receive: Actor.Receive = runRoute(route)

    def route: Route = {
      complete(name)
    }
  }

  /**
   * 此 Actor 对URL 的检查很严格，对所有发送给他的 RestRequest 检查其 url，
   * 和自己的URL完全一致的发送 200 响应，否则发送 404
   * @param name  响应名称
   */

  class StrictActor(name: String) extends RestServiceActor {

    def receive: Actor.Receive = runRoute(route)

    def route: Route = {

      pathEndOrSingleSlash {
        complete(name)
      }
    }
  }

  /**
   * 此 Actor ，对所有发送给他的 RestRequest 检查其 url，
   * 和自己的URL完全一致的发送 200 响应，否则建立对应的子Actor,并将请求转发给它
   * @param name  响应名称
   */
  class AutoActor(name: String) extends RestServiceActor with Requester {
    def receive: Actor.Receive = runRoute(route)
    import zzb.rest.StatusCodes._

    implicit def childByName: String ⇒ Either[(StatusCode, String), ActorRef] = childName ⇒
      if (childName != "reject")
        Right(context.child(childName).getOrElse(context.actorOf(Props(new AutoActor(childName)), childName)))
      else
        Left((BadRequest, "error"))

    def route: Route = {
      pathEndOrSingleSlash {
        complete(name)
      } ~
        forwardChild
    }
  }

}
