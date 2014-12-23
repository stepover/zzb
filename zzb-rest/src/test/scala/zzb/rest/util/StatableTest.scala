package zzb.rest.util

import org.scalatest.{ BeforeAndAfterAll, WordSpec }
import org.scalatest.MustMatchers
import zzb.rest._
import akka.actor.{ Actor, Props, ActorSystem }
import akka.pattern._
import spray.util._
import akka.util.Timeout
import scala.concurrent.duration._

/**
 * Created by Simon on 2014/3/25
 */
class StatableTest extends WordSpec
  with MustMatchers
  with Requester with BeforeAndAfterAll {

  implicit val system = ActorSystem()

  override protected def beforeAll() {

    system.actorOf(Props(new AccountActor2 with StatableActor), "account2")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  "StatableActor " must {
    val helloActor = system.actorOf(Props(new HelloActor with StatableActor), "hello")
    implicit val timeout = Timeout(5000000 milliseconds)
    "can count message" in {
      helloActor ! "hello"
      val stat = helloActor.ask(RequestStat).await
      stat.asInstanceOf[MessageStat].count mustBe 1
    }

    "rest actor can count message" in {
      val account = Account("simon", 39)

      val res1 = (Get("/account2") ~> doRequest).await
      res1.status must equal(StatusCodes.OK)
      res1.entity.data must equal(account)

      val res2 = (Get("/account2/stat") ~> doRequest).await
      res2.status must equal(StatusCodes.OK)
      val stat = res2.entity.data.asInstanceOf[MessageStat]
      stat.count mustBe 2
    }
  }

}

class HelloActor extends Actor {
  override def receive = {
    case "hello" ⇒
      println("ok")
  }
}
