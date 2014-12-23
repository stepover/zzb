package zzb.shell

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import zzb.shell.Action._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-10
 * Time: 下午2:05
 * Copyright baoxian.com 2012~2020
 */
class ActionStatExeTest extends WordSpec with MustMatchers {

  Shell.init(ConfigFactory.load("defaultShellRemote.conf"),ActorSystem())

  val system = Shell.system
  import system.dispatcher



  "do action by Action" must {
    "Action's 'map' work well when action success" in {

      implicit val res = StatResult()
      val a_echo = EchoAction(101)
      val double_f = stat {
        for {
          a <- a_echo
        } yield a * 2
      }

      double_f.onSuccess {
        case v: Int => v must equal(202)
      }

      res.future.onSuccess {
        case stats => stats.head.onSuccess {
          case stat => stat.isSucceed must equal(true)
        }
      }
      Await.result(res.future, Duration(2, SECONDS))

    }

    "Action's 'map' work well when action failed" in {

      implicit val res = new StatResult

      val a_echo = EchoAction(1)
      val double_f = stat {
        for {
          a <- a_echo
        } yield a * 2
      }

      double_f.onSuccess {
        case v: Int => throw new Exception("see ghost 4")
      }

      double_f.onFailure {
        case ex: ActionException => assert(ex.getCause.getMessage == EchoAction.errMsg)
        case ex: Throwable => throw new Exception("see ghost 5")
      }

      res.future.onSuccess {
        case stats => stats.head.onSuccess {
          case stat => stat.isSucceed must equal(false)
        }
      }

      val stats_lf = Await.result(res.future, Duration(2, SECONDS))
      val stats_fl = Future.sequence(stats_lf)
      val stats_l = Await.result(stats_fl, Duration(2, SECONDS))
      stats_l.size must equal(1)
    }
    "Action's 'flatmap' work well when action success" in {

      implicit val res = new StatResult

      val a_echo1 = EchoAction(101)
      val a_echo2 = EchoAction(102)
      val dv_f = stat {
        for {
          v1 <- a_echo1
          v2 <- a_echo2
          dv <- DoubleAction(v1)
        } yield dv
      }

      dv_f.onSuccess {
        case dv => dv must equal(202)
      }

      res.future.onSuccess {
        case stats => assert(stats.size == 3)
      }

      val stats_lf = Await.result(res.future, Duration(2, SECONDS))
      val stats_fl = Future.sequence(stats_lf)
      val stats_l = Await.result(stats_fl, Duration(2, SECONDS))
      stats_l.size must equal(3)
    }

    "Action's 'flatmap' work well when action failed " in {

      implicit val res = new StatResult
      val a_echo = EchoAction(1)

      val dv_f = stat {
        for {
          v <- a_echo
          dv <- DoubleAction(v)
        } yield dv
      }

      dv_f.onSuccess {
        case _ => throw new Exception("see ghost 6")
      }
      res.future.onSuccess {
        case stats => assert(stats.size == 1)
      }
    }

    "Action's 'flatmap' work well when action failed in nest func" in {
      def oneValue = {
        implicit val res = new StatResult
        val a_echo = EchoAction(1)
        stat {
          for {
            v <- a_echo
          } yield v
        }
      }
      implicit val res = new StatResult
      val theValue = oneValue
      val dv_f = stat {
        for {
          v <- theValue
          dv <- DoubleAction(v)
        } yield dv
      }

      dv_f.onSuccess {
        case _ => throw new Exception("see ghost 7")
      }
      res.future.onSuccess {
        case stats => assert(stats.size == 0)
      }
    }
    "Action's 'flatmap' work well when not need stat" in {

      val a_echo = EchoAction(101)
      val dv_f = for {
        v <- a_echo
        dv <- DoubleAction(v)
      } yield dv

      dv_f.onSuccess {
        case dv => dv must equal(202)
      }
    }
  }
}


