package zzb.shell.remote

import zzb.shell.{TaskSyncMode, Task}
import akka.actor.{ActorRef, PoisonPill, Props, Actor}
import zzb.shell.remote.ShellProtocol._
import akka.pattern._
import scala.util.{Failure, Success}
import scala.concurrent.{Promise, Await}
import akka.util.Timeout
import scala.concurrent.duration._
import java.io.{OutputStream, PrintStream}
import zzb.shell.remote.ShellDaemon.SessionCheck


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-17
 * Time: 下午12:00
 * Copyright baoxian.com 2012~2020
 */

class RemoteLogin extends Task {
  override def usage = "usage: remote-login systemName@host:port user password"

  override def checkArgs = args.size == 3

  override def syncMode = TaskSyncMode.MustSync

  import system.dispatcher

  val List(url, user, pass, _*) = args

  val sessionManagerActor = system.actorSelection(s"akka.tcp://$url/user/sessionManager")

  val connector = system.actorOf(Props[ShellConnectorActor])

  //  val system = shell.system




  var outToRemote_p = Promise[PrintStream]()

  var sessionId: String = _

  var isLost = false


  implicit val timeout = Timeout(30.seconds)

  val res = sessionManagerActor ? Login(user, pass, connector)

  res.onComplete {
    case Failure(ex) => outToRemote_p.failure(ex)
    case Success(LoginFailed) =>
      outToRemote_p.failure(new Exception("login failed: wrong 'user.password' !"))
    case Success(LoginSuccess(sid, peer)) =>
      println("login success!")
      sessionId = sid
      val outToRemote = new PrintStream(new ActorOutputStream(peer))
      outToRemote_p.success(outToRemote)
      connector ! ConnectionSession(sid, peer, shell.output.out, this)

    case Success(v) => outToRemote_p.failure(new Exception("unknown error"))
  }

  try {
    val outToRemote = Await.result(outToRemote_p.future, Duration.Inf)
    println("in remote shell")
    var command: String = shell.input.readClearCommand.trim

    while (command != "exit" && !isLost) {
      outToRemote.println(command)
      command = shell.input.readClearCommand.trim
    }
    outToRemote.close()
    if (!isLost) //主动退出
      sessionManagerActor ! Logout(sessionId)
    connector ! PoisonPill

  } catch {
    case ex: Throwable => println(ex.getMessage)
  }


}

case class ConnectionSession(sid: String, peer: ActorRef, outToConsole: OutputStream, task: RemoteLogin) {
  var lastPongTime = System.currentTimeMillis
}

class ShellConnectorActor extends Actor {
  var session: ConnectionSession = _
  val system = context.system

  import system.dispatcher

  val checkSession = system.scheduler.schedule(5.seconds, 5.seconds, self, SessionCheck)

  private val pong_timeout: Long = 90 * 1000L //unit ms，90 seconds

  def receive: Actor.Receive = {
    case Data(d: Int) if session != null =>
      session.outToConsole.write(d)
      session.outToConsole.flush()
      session.lastPongTime = System.currentTimeMillis

    case SessionLost(sid, reason) if session != null =>
      val p2c = new PrintStream(session.outToConsole)
      p2c.println(s"session lost reason[$reason],press ‘Enter’ to exit....")
      //val d = new PipedInputStream(session.ins)
      session.task.isLost = true

    case SessionCheck if session != null =>
      val now = System.currentTimeMillis
      if (now - session.lastPongTime > pong_timeout) {
        val p2c = new PrintStream(session.outToConsole)
        p2c.println(s"remote session lost, reason:[ net error ],press ‘Enter’ to exit....")
        session.task.isLost = true
      }
      else
        session.peer ! Ping(session.sid)

    case Pong if session != null =>
      session.lastPongTime = System.currentTimeMillis

    case s: ConnectionSession => session = s
  }

  override def postStop() = checkSession.cancel()

  override def preRestart(reason: Throwable, message: Option[Any]) {
    self ! session
  }
}
