package zzb.rest

import akka.actor.{ ActorSystem, Props }
import akka.util.Timeout
import org.scalatest.{ BeforeAndAfterAll, MustMatchers, WordSpec }
import spray.http.Uri
import spray.util._
import zzb.rest.RestResponse._

import scala.concurrent.duration._

/**
 * Created by Simon on 2014/6/14
 */
class RestResponseTransTest extends WordSpec
  with MustMatchers
  with Caller with BeforeAndAfterAll {

  implicit val system = ActorSystem("route")

  override protected def beforeAll() {
    system.actorOf(Props[AccountActor], "account")
    system.actorOf(Props[AccountActor2], "account2")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(1500 milliseconds)

  implicit val rootUri = Uri("")

  import system.dispatcher

  val account = Account("simon", 39)

  "RestResponse 类型转换 " must {
    "能够进行类型直接强制转换，类型不正确会抛出 ResponseWrongType 异常" in {
      doPost("/account/name", account).map(to[String]).await mustBe "simon"
      doPost("/account/age", account).map(to[Int]).await mustBe 39
      doPost("/account/tname", account).map(to[String]).await mustBe "simon"

      intercept[ResponseWrongType] {
        doPost("/account/name", account).map(to[Int]).await
      }
    }
    "可以指定缺省值" in {
      doPost("/account/name", account).map(
        toOrElse[Int](_, 100) //返回的类型是String,不是期待的Int,会使用缺省值
        ).await mustBe 100
    }

    "响应的状态符合要求才使用指定的缺省值" in {
      doPost("/account/noThis", account).map(
        toOrElseInStatus[String](_, "jack", StatusCodes.NotFound) //响应404 ,会使用缺省值
        ).await mustBe "jack"

      intercept[RequestFailed] {
        doPost("/account/noThis", account).map(
          toOrElseInStatus[String](_, "jack", StatusCodes.BadRequest) //响应404,不是400,不会使用缺省值
          ).await
      }

      doPost("/account/noThis", account).map(
        toOrElseNotStatus[String](_, "jack", StatusCodes.BadRequest) //响应404 ,不是400,会使用缺省值
        ).await mustBe "jack"
    }

    "可以指定类型转换函数" in {
      implicit val anyToString: PartialFunction[Any, String] = {
        case i: Int ⇒ i.toString
      }
      doPost("/account/age", account).map(to[String]).await mustBe "39" //整数类型能够用上面的转换函数转换

      intercept[ResponseWrongType] {
        doPost("/account/age", account).map(to[Boolean]).await //没有到Boolean 的转换函数
      }

    }
  }

}
