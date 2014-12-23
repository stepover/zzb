package zzb.shell

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import scala.util.{Failure, Success}
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import spray.util._
import scala.concurrent.Future


/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-10-9
 * Time: 下午2:49
 * Copyright baoxian.com 2012~2020
 */
class ActionErrorTest extends WordSpec with MustMatchers {

  Shell.init(ConfigFactory.load("defaultShellRemote.conf"), ActorSystem())
  val system = Shell.system

  import system.dispatcher

  "when action generate error" must {
    " stat data can get exception msg" in {
      val ea = EchoAction(1)
      val stat = ea.stat.await
      stat.isSucceed mustBe false
      stat.ex.get.getMessage mustBe EchoAction.errMsg
    }

  " error can spread to cascade future" in {
    //val ea = EchoAction(1)
    val dv_f: Future[Int] = for {
      v <- EchoAction(1)
      dv <- DoubleAction(v)
    } yield dv

    intercept[ActionException]{
      dv_f.await
    }
//
//
//
//    dv_f.onComplete {
//      case Success(s) => throw new Exception("see ghost 2")
//      case Failure(ex: ActionException) => assert(ex.getCause.getMessage == EchoAction.errMsg)
//      case Failure(ex) => ex.printStackTrace(); throw new Exception("see ghost 3")
//    }
  }
}

}


case class EchoAction(v: Int) extends Action[Int] {

  def func(): Int = {
    if (v >= 100) v
    else throw new Exception(EchoAction.errMsg)
  }
}

object EchoAction {
  val errMsg = "echo value less than 100"
}

case class DoubleAction(v: Int) extends Action[Int] {

  def func(): Int = {
    v * 2
  }
}
