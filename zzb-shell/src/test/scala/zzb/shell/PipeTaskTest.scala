package zzb.shell

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import scala.concurrent.{Future, Await}
import wash.shell.ConsolePrintStream
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-5
 * Time: 下午3:55
 * Copyright baoxian.com 2012~2020
 */

object Util {

  class FutureWrap[+A](underlying: Future[A]) {

    def await(implicit timeout: Duration = Duration(1, MINUTES)): A =
      Await.result(underlying, timeout)

    def ready(implicit timeout: Duration = Duration(1, MINUTES)): Future[A] =
      Await.ready(underlying, timeout)
  }

  implicit def future2Wrap[V](f: Future[V]) = new FutureWrap(f)
}

import Util._
class PipeTaskTest extends WordSpec with MustMatchers {

  Shell.init(ConfigFactory.load("defaultShellRemote.conf"),ActorSystem())

  "Pipe Task " must {
    Task.parseOne("NodeOne", "zzb.shell.NodeOne")
    Task.parseOne("NodeBack", "zzb.shell.NodeBack")
    Task.parseOne("NodeError", "zzb.shell.NodeError")
    Task.parseOne("NodeErrorCaller", "zzb.shell.NodeErrorCaller")
    Task.parseOne("NodeException", "zzb.shell.NodeException")
    Task.parseOne("NodeExceptionCaller", "zzb.shell.NodeExceptionCaller")

    Task.parseOne("GetNowTime", "zzb.shell.GetNowTime")

    val shell = Shell("hi","hi",notUseIoIn = true,sync = false)


    "Pipable Task can be create " in {
      val task = Task("NodeOne", null, Nil, true)

      Thread.sleep(100)
      val result = task.out.asInstanceOf[ConsolePrintStream].content()

      result must equal("hello")
    }
    "fromOther can work" in {
      val task = Task("NodeBack", shell, Nil, isAsync = true)

      Thread.sleep(300)
      val result = task.out.asInstanceOf[ConsolePrintStream].content()

      result must equal("hello")
    }

    "when use fromOther,other task error can be got by caller " in {
      val task = Task("NodeErrorCaller", shell, Nil, isAsync = true)

      Thread.sleep(300)
      val result = task.out.asInstanceOf[ConsolePrintStream].content()

      result must equal("hello")
    }
    "when use fromOther,other task exception can be got by caller " in {
      val task = Task("NodeExceptionCaller", shell, Nil, isAsync = true)

      Thread.sleep(300)
      val result = task.out.asInstanceOf[ConsolePrintStream].content()

      result must equal("hello")
    }
    "result of task can be cached" in {
      val f0 = shell.fromOther[Long]("GetNowTime").await
      Thread.sleep(200)
      val f1 = shell.fromOther[Long]("GetNowTime").await
      f1 must equal(f0)     //使用缓存中的数据
      val f2 = shell.fromOther[Long]("GetNowTime",forceUpdate = true).await
      f2 must (be > f1)     //缓存被强制更新了
    }

    shell.requestExeCmd("exit")


  }
}


class NodeOne extends Task with Pipable[String] {

  pipeToNext("hello")

  print("hello")
}


class NodeBack extends Task {

  import system.dispatcher

  val f = fromOther[String]("NodeOne")
  f.onSuccess {
    case v => Console.withOut(out) {
      print(v)
    }
  }
}

class NodeErrorCaller extends Task with Pipable[String] {
  import system.dispatcher
  val f = fromOther[String]("NodeError")
  f.onFailure {
    case e:Throwable => Console.withOut(out) {
      print(e.getMessage)
    }
  }
}

class NodeError extends Task with Pipable[String] {
  errorToNext(new Exception("hello"))
}

class NodeException extends Task with Pipable[String] {
  throw new Exception("hello")
}

class NodeExceptionCaller extends Task with Pipable[String] {
  import system.dispatcher
  val f = fromOther[String]("NodeException")
  f.onFailure {
    case e:Throwable => Console.withOut(out) {
      print(e.getMessage)
    }
  }
}

class GetNowTime  extends Task with Pipable[Long]{
  pipeToNext(System.currentTimeMillis)
}