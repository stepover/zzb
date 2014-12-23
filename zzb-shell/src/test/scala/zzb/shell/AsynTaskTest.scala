package zzb.shell

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import scala.util.{ Failure, Success }
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-25
 * Time: 下午2:01
 * Copyright baoxian.com 2012~2020
 */
class AsynTaskTest extends WordSpec with MustMatchers {

  Shell.init(ConfigFactory.load("defaultShellRemote.conf"),ActorSystem())

  val system = Shell.system
  import system.dispatcher


  "asyn task " must {
    " run in different thread " in {
      val mainThreadId = Thread.currentThread().getId
      QueryThreadId(mainThreadId).future.andThen {

        case Success(subThreadId) ⇒ mainThreadId must not equal (subThreadId)

        case Failure(_)           ⇒ throw new Exception("error")
      }
    }

    "can get task time span " in {

      LongTimeWore("simon").stat.andThen {

        case Success(s) ⇒ s.timeSpan must (be > (1950L))

        case Failure(_)    ⇒ throw new Exception("error")
      }

    }
  }

}

case class QueryThreadId(id: Long) extends Action[Long] {
  def func(): Long = {
    Thread.currentThread().getId
  }
}

case class LongTimeWore(name: String) extends Action[String] {
  def func(): String = {
    Thread.sleep(2000)
    name
  }
}

