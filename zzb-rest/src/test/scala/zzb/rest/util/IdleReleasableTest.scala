package zzb.rest.util

import org.scalatest.{ BeforeAndAfterAll, WordSpec }
import org.scalatest.MustMatchers
import zzb.rest._
import akka.actor.{ ActorNotFound, Props, ActorSystem, Actor }
import spray.util._
import akka.util.Timeout
import scala.concurrent.duration._

/**
 * Created by Simon on 2014/3/27
 */
class IdleReleasableTest extends WordSpec
  with MustMatchers
  with Requester with BeforeAndAfterAll {
  implicit val system = ActorSystem()

  override protected def beforeAll() {

  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(5000 milliseconds)

  "Idle Actor " must {
    "can be release after maxIdle time" in {
      system.actorOf(Props(new AutoReleaseActor(3) with IdleReleasable with StatableActor), "auto")
      val act = system.actorSelection("akka://default/user/auto")
      act.resolveOne().await

      Thread.sleep(6000)

      intercept[ActorNotFound] {
        act.resolveOne().await
      }
    }
  }
}

class AutoReleaseActor(val maxIdle: Int) extends Actor {
  def preIdleRelease: Boolean = true

  def receive = {
    case "hello" ⇒ sender ! "ok"
  }
}