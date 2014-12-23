package zzb.shell.remote

import com.typesafe.config.ConfigFactory
import akka.actor._
import zzb.shell.Shell
import zzb.shell.remote.ShellProtocol._
import zzb.shell.remote.ShellProtocol.SessionLost
import scala.Some
import zzb.shell.remote.ShellProtocol.LoginSuccess
import zzb.shell.remote.ShellProtocol.Login
import java.io.{OutputStream, PrintStream, PipedInputStream, PipedOutputStream}
import com.typesafe.scalalogging.slf4j.Logging


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-17
 * Time: 下午12:03
 * Copyright baoxian.com 2012~2020
 */
object ShellDaemon extends Logging {

  private var userName: String = _
  private var passWord: String = _
  private var action_timeout: Long = 10 * 60 * 1000L
  //unit ms，10minute
  private val ping_timeout: Long = 90 * 1000L //unit ms，90 seconds

  private var sessionManager_ : ActorRef = _

  private var appName_ = "shell-daemon"

  private var running_ = false

  private var cancell: Cancellable = _


  def install(appName: String): ActorRef = {
    if (running) {
      logger.info("Shell daemon is already running!")
      sessionManager_
    } else {
      appName_ = appName

      //装载缺省值配置,缺省值配置文件在资源包中
      val defaultShellConfig = ConfigFactory.load("defaultShellRemote.conf").getConfig("daemon")
      val daemonConfig = try {
        Shell.config.getConfig("daemon").withFallback(defaultShellConfig)
      } catch {
        case ex: Throwable ⇒ defaultShellConfig
      }

      userName = daemonConfig.getString("username")
      passWord = daemonConfig.getString("password")
      action_timeout = daemonConfig.getLong("session-timeout") * 1000
      sessionManager_ = Shell.system.actorOf(Props[SessionManagerActor], name = "sessionManager")
      logger.info("Shell remote daemon installed,can accept remote shell login...")
      logger.info(sessionManager_.path.toString)

      import scala.concurrent.duration._

      val system = Shell.system
      import system.dispatcher

      cancell = system.scheduler.schedule(5.seconds, 3.seconds, sessionManager_, SessionCheck)
      running_ = true
      sessionManager_
    }
  }

  def remove() = {

    if (running) {
      logger.info("Shell remote daemon removed,will refuse remote shell login...")
      cancell.cancel()
      sessionManager_ ! PoisonPill
      running_ = false
      //RemoteEnv.requestShutdown()
    }

  }

  def running = running_

  val sessions = scala.collection.mutable.HashMap[String, ClientSession]()

  case class ClientSession(sid: String, userName: String,
                           peer: ActorRef, me: ActorRef, shell: Shell,
                           var lastActionTime: Long, var lastPingTime: Long)

  case class SID(id: String)

  class ShellWorkerActor extends Actor with ActorLogging {

    var sid: String = _
    var session: ClientSession = _
    var outToRemote: OutputStream = _

    def receive: Actor.Receive = {
      case SID(id) =>
        sid = id
        session = sessions(sid)
        outToRemote = session.shell.pipeToIn.get

      case Data(d: Int) if session != null =>
        outToRemote.write(d)
        val now = System.currentTimeMillis
        session.lastActionTime = now
        session.lastPingTime = now

      case _ => ()
    }

    override def preRestart(reason: Throwable, message: Option[Any]) {
      self ! SID(sid)
    }
  }

  class SessionManagerActor extends Actor with ActorLogging {

    def receive: Actor.Receive = {
      case Login(name, pass, peer) ⇒
        if (name != userName || pass != passWord) sender ! LoginFailed
        else {
          val sid = java.util.UUID.randomUUID.toString
          val workerWriteTo = new PipedOutputStream()
          val shellIn = new PipedInputStream()
          workerWriteTo.connect(shellIn)
          val shellOut = new PrintStream(new ActorOutputStream(peer), true)
          val shell = Shell(appName_, appName_, shellIn, shellOut, shellOut,
            Some(new PrintStream(workerWriteTo)), sync = false, notUseIoIn = false)

          val me = context.actorOf(Props[ShellWorkerActor], s"shellWorker-$sid")

          sender ! LoginSuccess(sid, me)

          val now = System.currentTimeMillis
          val cs = ClientSession(sid, name, peer, me, shell, now, now)

          sessions(cs.sid) = cs
          me ! SID(sid)

          log.info(s"create new remote shell [sid:$sid] .")
        }


      case Logout(sid) if sessions.contains(sid) =>

        val s = sessions(sid)
        s.shell.pipeToIn("exit")
        s.me ! PoisonPill
        sessions.remove(sid)

      case SessionCheck ⇒ sessionTimeoutCheck()

      case Ping(sid) if sessions.contains(sid) =>
        sender ! Pong
        sessions(sid).lastPingTime = System.currentTimeMillis

      case KickAll =>
        for (sid <- sessions.keySet) {
          val s = sessions(sid)
          s.peer ! SessionLost(sid, s"remote service shutdown....")
          s.shell.pipeToIn("exit")
          s.me ! PoisonPill
        }
        sessions.clear()


      case _ ⇒ ()
    }

    private def sessionTimeoutCheck() {
      val now = System.currentTimeMillis
      for (sid <- sessions.keySet) {
        val s = sessions(sid)
        val idle = now - s.lastActionTime
        if (idle > action_timeout) {
          s.peer ! SessionLost(sid, s"${idle / 1000} seconds not interactive action.")
          log.info(s"remote shell [sid:$sid] stop,long time not interactive action.")
          s.shell.pipeToIn("exit")
          s.me ! PoisonPill
          sessions.remove(sid)
        }
        else if (now - s.lastPingTime > ping_timeout) {
          s.peer ! SessionLost(sid, "net error.")
          log.info(s"remote shell [sid:$sid] stop,net error.")
          s.shell.pipeToIn("exit")
          s.me ! PoisonPill
          sessions.remove(sid)
        }
//        else
//          s.peer ! Pong
      }
    }
  }

  case object SessionCheck

}
