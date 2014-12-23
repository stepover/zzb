package zzb.rest.http2akka

import org.scalatest.{ FreeSpec, BeforeAndAfterAll }
import org.scalatest.MustMatchers
import zzb.rest._
import akka.actor.Props
import akka.util.Timeout
import scala.concurrent.duration._
import spray.routing.Directives
import spray.testkit.ScalatestRouteTest
import spray.http._
import zzb.rest.routing.{ LooseActor, StrictActor, AutoActor }
import spray.http.MediaTypes._
import spray.http.HttpCharsets._
import spray.http.StatusCodes
import StatusCodes._
import zzb.rest.util.{ MessageStat, StatableActor }
import com.typesafe.config.{ ConfigValueFactory, Config }

/**
 * Created by Simon on 2014/3/20
 */
class Http2AkkaTest extends FreeSpec with MustMatchers with Directives with Http2AkkaDirectives with ScalatestRouteTest
  with BeforeAndAfterAll {

  override protected def beforeAll() {
    system.actorOf(Props[AccountActor], "account")
    system.actorOf(Props(new AccountActor2 with StatableActor), "account2")
    system.actorOf(Props(new LooseActor("loose")), "loose")
    system.actorOf(Props(new StrictActor("strict")), "strict")
    system.actorOf(Props(new AutoActor("auto")), "auto")

  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  override def testConfig: Config = {
    super.testConfig.withValue("akka.log-dead-letters-during-shutdown", ConfigValueFactory.fromAnyRef("off"))
  }
  implicit val timeout = Timeout(1000 milliseconds)

  "http2akka " - {

    "can accept request by uri" in {
      val res1 = Get("/loose") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "loose")
      }(res1)

      val res2 = Get("/strict") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "strict")
      }(res2)

      val res3 = Get("/auto") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "auto")
      }(res3)
    }

    "if request uri not found,will be send to parent path " in {
      val res1 = Get("/loose/nothis") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "loose")
      }(res1)

      val res2 = Get("/strict/nothis") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe NotFound
      }(res2)

      val res3 = Get("/auto/nothis") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "nothis")
      }(res3)

      val res4 = Get("/auto/nothis/no_again") ~> {
        akkaCompleteAs[String]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`text/plain`, `UTF-8`), "no_again")
      }(res4)
    }

    import spray.json._
    import spray.httpx.SprayJsonSupport._
    object MyJsonProtocol extends DefaultJsonProtocol {
      implicit val accountFormat = jsonFormat2(Account)
    }
    import MyJsonProtocol._

    val jsonValue = "{\n  \"name\": \"simon\",\n  \"age\": 39\n}"

    "object entity can be get " in {

      val res1 = Get("/account2") ~> {
        akkaCompleteAs[Account]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`application/json`, `UTF-8`), jsonValue)
        headers.size mustBe 1
      }(res1)

    }

    "object entity can be get,url changed " in {

      val res1 = Get("/account333") ~> mapRequest(req ⇒ req.copy(uri = "/account2")) {
        akkaCompleteAs[Account]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`application/json`, `UTF-8`), jsonValue)
      }(res1)

    }

    "object entity can be put " in {

      val res1 = HttpRequest(HttpMethods.PUT, "/account2", Nil, HttpEntity(ContentType(`application/json`, `UTF-8`), jsonValue)) ~>
        put {
          entity(as[Account]) {
            account ⇒
              akkaWithEntityCompleteAs[Account](account)
          }
        } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        status mustBe OK
        body mustBe HttpEntity(ContentType(`application/json`, `UTF-8`), jsonValue)
      }(res1)
    }

    "request count support" in {
      val res1 = Get("/account2/stat") ~> {
        akkaCompleteAs[MessageStat]
      } ~> runRoute
      Thread.sleep(timeout.duration.toMillis)
      check {
        val res = response
        status mustBe OK
      }(res1)
    }

    Thread.sleep(200)

  }

}
