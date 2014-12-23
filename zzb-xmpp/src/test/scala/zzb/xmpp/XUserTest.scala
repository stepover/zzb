package zzb.xmpp

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.{ConfigFactory, Config}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike, MustMatchers, WordSpec}
import zzb.xmpp.ebus._

/**
 * Created by Simon on 2014/8/7
 */
class XUserTest extends TestKit(ActorSystem("testSystem",ConfigFactory.load("xmpp.conf")))
with WordSpecLike
with MustMatchers
with ImplicitSender
with BeforeAndAfterAll {

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  val bus = XmppEventBus.default

  "Xmpp 配置" must {
    "可以找到用户配置" in {
      //自动会使用ActorSystem的配置解析XUserSettings
      def setting(implicit ss:XUserSettings) = ss
      setting.users.get("u1").isDefined mustBe true
      val cc = setting.users.get("u1").get
      cc.resource mustBe "verdict"
    }
  }

  "Xmpp user" must {

    "能够正确发布自己的状态信息" in {

      bus.subscribe(testActor, "/xuser/u1/status/self")
      //启动所有配置的 XMPP 账号
      XUser.start("u1")

      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println()
          println("got 'offline' msg from bus")
          status mustBe Offline
      }

      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println("got 'connecting' from bus")
          status mustBe Connecting
      }

      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println("got 'online' from bus")
          status mustBe Online
      }

    }

    "能够正确发布好友的上下线信息" in {
      bus.subscribe(testActor, "/xuser/u1/status/friends/fszb_biz2@imadmin.net/verdict")
      XUser.start("u2") //启动第二个用户，已经预先将 u1 和 u2 加为好友

      expectMsgPF() {
        case XStatus(path, jid, status) =>
          println()
          println(s"got $jid 'online' msg from bus")
          status mustBe Online
      }
    }

    "能够一对一聊天" in {

      //订阅 u1 收到的聊天消息
      bus.subscribe(testActor, "/xuser/u1/chat-from")

      // u2 对 u1 说一句话
      XUser("u2").sendMsgTo("fszb_biz1@imadmin.net/verdict","hello world")

      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          body mustBe "hello world"
      }

      // u1 对 u2 说一句话
      XUser("u1").sendMsgTo("fszb_biz2@imadmin.net/verdict","echo hello baby")

      // u1 应该收到回应
      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          body mustBe "hello baby"
      }
      bus.unsubscribe(testActor)
    }

    "支持对消息 Subject 的订阅(使用正则表达式)" in {
      //订阅 u1 收到的聊天消息
      bus.subscribe(testActor, "^/xuser/u1/chat-from/fszb_biz2@imadmin.net/verdict/.*/welcome$")

      // u2 对 u1 说一句话
      XUser("u2").sendMsgTo("fszb_biz1@imadmin.net/verdict","hello world","welcome")

      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          subject mustBe "welcome"
          body mustBe "hello world"
      }

      // u1 对 u2 说一句话
      XUser("u1").sendMsgTo("fszb_biz2@imadmin.net/verdict","echo hello baby","welcome")

      // u1 应该收到回应
      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          subject mustBe "welcome"
          body mustBe "hello baby"
      }
      bus.unsubscribe(testActor)
    }

    "支持对消息 thread 的订阅" in {
      //订阅 u1 收到的聊天消息
      bus.subscribe(testActor, "/xuser/u1/chat-from/fszb_biz2@imadmin.net/verdict/54321")

      // u2 对 u1 说一句话
      XUser("u2").sendMsgTo("fszb_biz1@imadmin.net/verdict","hello world","welcome","54321")

      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          subject mustBe "welcome"
          thread mustBe "54321"
          body mustBe "hello world"
      }

      // u1 对 u2 说一句话
      XUser("u1").sendMsgTo("fszb_biz2@imadmin.net/verdict","echo hello baby","welcome","54321")

      // u1 应该收到回应
      expectMsgPF() {
        case XMessageIn(path, from,to , body,subject,thread) =>
          from mustBe "fszb_biz2@imadmin.net/verdict"
          subject mustBe "welcome"
          thread mustBe "54321"
          body mustBe "hello baby"
      }
      bus.unsubscribe(testActor)
    }

//    "支持对消息路径的 正则表达式 的订阅" in {
//      //订阅 u1 收到的聊天消息
//      bus.subscribe(testActor, "^/xuser/u1/chat-from/fszb_biz2@imadmin.net/verdict/.*/54321$")
//
//      // u2 对 u1 说一句话
//      XUser("u2").sendMsgTo("fszb_biz1@imadmin.net/verdict","hello world","welcome","54321")
//
//      expectMsgPF() {
//        case XMessageIn(path, from,to , body,subject,thread) =>
//          from mustBe "fszb_biz2@imadmin.net/verdict"
//          subject mustBe "welcome"
//          thread mustBe "54321"
//          body mustBe "hello world"
//      }
//
//      // u1 对 u2 说一句话
//      XUser("u1").sendMsgTo("fszb_biz2@imadmin.net/verdict","echo hello baby","welcome","54321")
//
//      // u1 应该收到回应
//      expectMsgPF() {
//        case XMessageIn(path, from,to , body,subject,thread) =>
//          from mustBe "fszb_biz2@imadmin.net/verdict"
//          subject mustBe "welcome"
//          thread mustBe "54321"
//          body mustBe "hello baby"
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
