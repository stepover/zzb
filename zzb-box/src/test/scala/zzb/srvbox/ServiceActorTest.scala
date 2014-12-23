package zzb.srvbox

import akka.testkit.{ ImplicitSender, TestActorRef, TestKit }
import akka.actor.{ Props, ActorSystem }
import org.scalatest.{ Ignore, WordSpecLike }
import org.scalatest.MustMatchers
import zzb.StopSystemAfterAll

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-13
 * Time: 下午2:52
 * Copyright baoxian.com 2012~2020
 */
class ServiceActorTest extends TestKit(ActorSystem("testSystem"))
    with WordSpecLike
    with MustMatchers
    with ImplicitSender
    with StopSystemAfterAll {

  import zzb.srvbox.SrvManageProtocol._

  "A Service Actor" must {

    val boxActor = system.actorOf(Props[BoxActor], "boxActor")

    boxActor ! Register("S1", "zzb.srvbox.S1Service", sharedActorSystem = true)

    val serviceActor = system.actorSelection("/user/boxActor/S1")

    "keep service name and class name when got Register message" in {

      serviceActor ! Register("S1", "zzb.srvbox.S1Service", sharedActorSystem = true)
      serviceActor ! "query"
      expectMsgPF() {
        case ServiceStatus(name, running) ⇒
          name must be("S1")
          running mustBe false
      }
    }
    "start service after got the 'start' command" in {
      serviceActor ! "start"

      expectMsgPF() {
        case ServiceStatus(name, running) ⇒
          name must be("S1")
          running mustBe true
      }
    }
    "stop service after got the 'stop' command" in {
      serviceActor ! "stop"

      expectMsgPF() {
        case ServiceStatus(name, running) ⇒
          name must be("S1")
          running mustBe false
      }
    }

  }

}

