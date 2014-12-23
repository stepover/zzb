package zzb.shell

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{WordSpecLike, BeforeAndAfterAll, MustMatchers}
import zzb.shell.remote.ShellProtocol._
import zzb.shell.remote.ShellProtocol.Login
import zzb.shell.remote.ShellProtocol.LoginSuccess
import zzb.shell.remote.ShellDaemon
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-17
 * Time: 下午2:38
 * Copyright baoxian.com 2012~2020
 */
class DaemonTest extends TestKit(ActorSystem("testSystem")) with WordSpecLike
with MustMatchers
with ImplicitSender
with BeforeAndAfterAll {

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  "inner shell " must {

    Shell.init(ConfigFactory.load("defaultShellRemote.conf"), system)

    val managerActor = ShellDaemon.install("test")

    "can login from remote" in {

      val wait = 3.seconds

      managerActor ! Login("not_me", "wrongPass", self)

      expectMsgPF(wait,"no this user , login failed") {
        case LoginFailed ⇒ ()
      }

      managerActor ! Login("admin", "wrongPass", self)
      expectMsgPF(wait,"wrong password，login failed") {
        case LoginFailed ⇒ ()
      }

      managerActor ! Login("admin", "baoxian.com!@#$%", self)
      expectMsgPF(wait,"login success") {
        case LoginSuccess(sid, peer) ⇒
          sid.length must equal(36)
      }
      expectMsgPF(wait,"received data") {
        case Data(d) ⇒ ()

      }
    }
  }

}
