package zzb.xmpp

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import zzb.xmpp.ebus._

/**
 * Created by Simon on 2014/8/13
 */
class XRoomPrivateChatTest  extends TestKit(ActorSystem("testSystem", ConfigFactory.load("xmpp.conf")))
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

    "可以私聊" in {

      bus.subscribe(testActor, s"/xuser/u1/status/$roomName")

      XUser.start("u1") //u1 登录并自动进入房间
      expectMsgPF() {
        case  XStatus(_, jid,Online) =>
          println(s"<u1> got $jid enter room msg from bus")
          //jid mustBe "u1"
      }
      bus.unsubscribe(testActor)

      bus.subscribe(testActor, s"/xuser/u2/status/$roomName")

      XUser.start("u2") //u2 登录并自动进入房间
      expectMsgPF() {
        case  XStatus(_, jid,Online) =>
          println(s"<u2> got $jid enter room msg from bus")
      }
      expectMsgPF() {
        case  XStatus(_, jid,Online) =>
          println(s"<u2> got $jid enter room msg from bus")
      }
      bus.unsubscribe(testActor)


      //订阅 u2 收到的房间私聊消息
      bus.subscribe(testActor, s"/xuser/u2/chat-from/$roomName")

      //以 u2 的身份向 u1 发送私聊消息， "echo " 开头的消息会自动回复
      XRoom("u2",roomName).sendMsgTo("u1","echo hello baby!")

      //接收 u1 给 u2 的自动回复(去掉了 "echo ")
      expectMsgPF() {
        case XMessageIn(path, from,to,body, subject,thread) =>
          //println(s"got $jid 'offline' from bus")
          body mustBe "hello baby!"
      }

      bus.unsubscribe(testActor)
    }

    "可以针对 subject 订阅私聊(使用正则表达式)" in {
      //订阅 u2 收到的房间私聊消息,指定发送人为u1,主题为 welcome
      bus.subscribe(testActor, s"^/xuser/u2/chat-from/$roomName/u1/.*/welcome" + "$")

      //以 u2 的身份向 u1 发送私聊消息， "echo " 开头的消息会自动回复
      XRoom("u2",roomName).sendMsgTo("u1","echo hello baby!","welcome")

      //接收 u1 给 u2 的自动回复(去掉了 "echo ")
      expectMsgPF() {
        case XMessageIn(path, from,to,body, subject,thread) =>
          subject mustBe "welcome"
          body mustBe "hello baby!"
      }
      bus.unsubscribe(testActor)
    }

    "可以针对 thread 订阅私聊" in {
      //订阅 u2 收到的房间私聊消息,指定发送人为u1,主题为 welcome,thread 为 54321
      bus.subscribe(testActor, s"/xuser/u2/chat-from/$roomName/u1/54321")

      //以 u2 的身份向 u1 发送私聊消息， "echo " 开头的消息会自动回复
      XRoom("u2",roomName).sendMsgTo("u1","echo hello baby!","welcome","54321")

      //接收 u1 给 u2 的自动回复(去掉了 "echo ")
      expectMsgPF() {
        case XMessageIn(path, from,to,body, subject,thread) =>
          subject mustBe "welcome"
          body mustBe "hello baby!"
      }
      bus.unsubscribe(testActor)
    }

//    "可以针对 正则表达式 订阅私聊" in {
//      //订阅 u2 收到的房间私聊消息,指定发送人为u1,主题为 welcome,thread 为 54321
//      bus.subscribe(testActor, s"^/xuser/u2/chat-from/$roomName/u1/.*/54321"+"$")
//
//      //以 u2 的身份向 u1 发送私聊消息， "echo " 开头的消息会自动回复
//      XRoom("u2",roomName).sendMsgTo("u1","echo hello baby!","welcome","54321")
//
//      //接收 u1 给 u2 的自动回复(去掉了 "echo ")
//      expectMsgPF() {
//        case XMessageIn(path, from,to,body, subject,thread) =>
//          subject mustBe "welcome"
//          body mustBe "hello baby!"
//      }
//      bus.unsubscribe(testActor)
//    }

    "能够正常下线" in {

      bus.subscribe(testActor, "/xuser/u1/status/self")
      XUser.stop("u1")

      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println(s"got $jid 'offline' from bus")
          status mustBe Offline
      }
      bus.unsubscribe(testActor)

      bus.subscribe(testActor, "/xuser/u2/status/self")
      XUser.stop("u2")
      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println(s"got $jid 'offline' from bus")
          status mustBe Offline
      }
    }
  }
}
