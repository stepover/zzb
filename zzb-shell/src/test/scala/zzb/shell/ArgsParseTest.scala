package zzb.shell

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-12-12
 * Time: 下午1:31
 * Copyright baoxian.com 2012~2020
 */
class ArgsParseTest extends WordSpec with MustMatchers {
  Shell.init(ConfigFactory.load("defaultShellRemote.conf"),ActorSystem())


  "Args Parse" must {

    Task.parseOne("argsParse", "zzb.shell.ParseAgrs")

    "args " in {
      val shell = Shell("test","test",notUseIoIn = true,sync = false)

      shell.requestExeCmd("argsParse name=Simon")
      shell.requestExeCmd("exit")
      Thread.sleep(500)
    }
  }


}



class ParseAgrs extends Task {

  Console.withOut(out){
    println(params)

    assert(params("name") == "Simon")
  }

}
