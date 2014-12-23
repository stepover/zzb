package zzb.srvbox

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.WordSpecLike
import org.scalatest.MustMatchers
import zzb.StopSystemAfterAll
import zzb.srvbox.SrvManageProtocol.ServiceStatus
import akka.util.Timeout
import scala.concurrent.duration._
import spray.util._

/**
 * Created by Simon on 2014/5/28
 */
class BoxBuilderTest extends TestKit(ActorSystem("testSystem"))
with WordSpecLike
with MustMatchers
with ImplicitSender
with StopSystemAfterAll {

  val (config, servicesOpts) = BoxBuilder.getSelectService(List("srvbox"))

  val boxBuilder = new BoxBuilder(system,config)


  implicit val timeout = Timeout(10 seconds)

  val startRes = boxBuilder.startBoxedServices(servicesOpts)

  Thread.sleep(1000)

  "BoxBuilder" must {
    "service started" in {

      val res = startRes.await
      res.length mustBe 2
      res(0).name mustBe "S1"
      res(0).running mustBe true

      res(1).name mustBe "S2"
      res(1).running mustBe true

      val s1 = system.actorSelection("/user/boxActor/S1")
      val s2 = system.actorSelection("/user/boxActor/S2")


      s1 ! "query"
      expectMsgPF() {
        case ServiceStatus(name, running) ⇒
          name mustBe "S1"
          running mustBe true
      }

      s2 ! "query"
      expectMsgPF() {
        case ServiceStatus(name, running) ⇒
          name mustBe "S2"
          running mustBe true
      }
    }
  }

}
