package zzb.srvbox.task

import zzb.shell.Task
import zzb.srvbox.SrvManageProtocol._
import akka.pattern.ask
import scala.util.{Failure, Success}
import akka.util.Timeout
import scala.concurrent.duration._

import zzb.shell.remote.ShellProtocol.KickAll

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-21
 * Time: 上午9:07
 * Copyright baoxian.com 2012~2020
 */

class List extends Task {
  implicit val timeout = Timeout(5.seconds)
  val boxActor = system.actorSelection("/user/boxActor")

  import system.dispatcher

  boxActor.ask(RequestList).onComplete {
    case Success(Services(services)) =>
      Console.withOut(out) {
        for (s <- services) {
          val status = if (s.running) "Running" else "Stoped"
          println(s" ${s.name} \t\t ------\t [$status]")
        }
      }

    case Success(v) => ()

    case Failure(ex) => Console.withOut(out) {
      println(ex.getMessage)
    }
  }
}


class Stop extends Task {
  implicit val timeout = Timeout(5.seconds)
  val boxActor = system.actorSelection("/user/boxActor")
  import system.dispatcher

  override def usage = "usage: srv-stop serviceName"
  override def checkArgs = args.size == 1

  val serviceName = args.head

  boxActor.ask(RequestStop(serviceName)).onComplete {
    case Success(ServiceNotExist) => Console.withOut(out) {
      println(s"not fouund service '$serviceName'")
    }
    case Success(ServiceStatus(name,running)) => Console.withOut(out) {
      val status = if (running) "Running" else "Stoped"
      println(s" $name \t\t ------\t [$status]")
    }
    case Success(v) => ()
    case Failure(ex) => Console.withOut(out) {
      println(ex.getMessage)
    }
  }
}

class Start extends Task {
  implicit val timeout = Timeout(5.seconds)
  val boxActor = system.actorSelection("/user/boxActor")

  override def usage = "usage: srv-start serviceName"
  override def checkArgs = args.size == 1

  val serviceName = args.head
  import system.dispatcher

  boxActor.ask(RequestStart(serviceName)).onComplete {
    case Success(ServiceNotExist) => Console.withOut(out) {
      println(s"not fouund service '$serviceName'")
    }
    case Success(ServiceStatus(name,running)) => Console.withOut(out) {
      val status = if (running) "Running" else "Stoped"
      println(s" $name \t\t ------\t [$status]")
    }
    case Success(v) => ()
    case Failure(ex) => Console.withOut(out) {
      println(ex.getMessage)
    }
  }
}

class Halt extends Task {
  implicit val timeout = Timeout(5.seconds)
  val boxActor = system.actorSelection("/user/boxActor")
  val sessionManager = system.actorSelection("/user/sessionManager")

  import system.dispatcher

  system.scheduler.scheduleOnce(1.seconds,
     new Runnable { override def run() = sessionManager ! KickAll })

  system.scheduler.scheduleOnce(5.seconds,
     new Runnable { override def run() = boxActor ! zzb.srvbox.SrvManageProtocol.Halt })
}


