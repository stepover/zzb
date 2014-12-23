package zzb.xmpp

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import zzb.xmpp.ebus._

/**
 * Created by Simon on 2014/8/12
 */
class XRoomTest extends TestKit(ActorSystem("testSystem", ConfigFactory.load("xmpp.conf")))
with WordSpecLike
with MustMatchers
with ImplicitSender
with BeforeAndAfterAll {

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  val bus = XmppEventBus.default

  val roomName = "zzb-xmpp@conference.imadmin.net"

  val u1 = "zzb-xmpp@conference.imadmin.net/u1"
  val u2 = "zzb-xmpp@conference.imadmin.net/u2"

  "XRoom" must {

    "后进Room者可以收到先进入者的出席通知" in {

      bus.subscribe(testActor, s"/xuser/u1/status/$roomName")

      XUser.start("u1") //u1 先登录并自动进入房间
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"<u1> got $jid enter room msg from bus")
      }
      bus.unsubscribe(testActor)

      //订阅u2 收到的用户进入房间事件
      bus.subscribe(testActor, s"/xuser/u2/status/$roomName")
      XUser.start("u2")
      //会收到两个进入房间的通知，一个是自己的，一个是u1的
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"----1--->  <u2> got $jid enter room msg from bus")
      }
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"----2--->  <u2> got $jid enter room msg from bus")
      }

      XUser.stop("u1")

      expectMsgPF() {
        case XStatus(_, jid,Offline) =>
          println(s"<u2> got $jid leave room msg from bus")
          jid mustBe u1
      }

      bus.unsubscribe(testActor)

      bus.subscribe(testActor, "/xuser/u2/status/self")
      XUser.stop("u2")
      expectMsgPF() {
        case XStatus(_, jid, Offline) =>
          println(s"got $jid 'offline' from bus")
      }
      bus.unsubscribe(testActor)
    }

    "自己掉线了，房间中其他人的离开通知也会正常发送（其他人并没有离开，但是因为自己掉线其他人等同与离开）" in {
      bus.subscribe(testActor, s"/xuser/u1/status/$roomName")

      XUser.start("u1") //u1 先登录并自动进入房间
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"<u1> got $jid enter room msg from bus")
      }
      bus.unsubscribe(testActor)

      //订阅u2 收到的用户进入房间事件
      bus.subscribe(testActor, s"/xuser/u2/status/$roomName")
      XUser.start("u2")
      //会收到两个进入房间的通知，一个是自己的，一个是u1的
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"----1---> <u2> got $jid enter room msg from bus")
      }
      expectMsgPF() {
        case XStatus(_, jid,Online) =>
          println(s"----2---> <u2> got $jid enter room msg from bus")
      }

      XUser.stop("u2")
      //应该收到两个离开通知
      expectMsgPF() {
        case XStatus(_, jid,Offline) =>
          println(s"----1---> got $jid leave room msg from bus")

      }
      expectMsgPF() {
        case XStatus(_, jid,Offline) =>
          println(s"----2---> got $jid leave room msg from bus")
      }

      bus.unsubscribe(testActor)

      Thread.sleep(1000) //停一会，让路径 Path(s"/xuser/u1/status") 的消息走完

      //订阅u1 收到的用户进出房间事件
      bus.subscribe(testActor, s"/xuser/u1/status/$roomName")
      XUser.stop("u1")
      //应该收到一个离开通知
      expectMsgPF() {
        case XStatus(p, jid,Offline) =>
          println(s"----3---> got $jid leave room msg from bus ： $p")
          jid mustBe u1
      }

      bus.unsubscribe(testActor)
    }

  }
}
