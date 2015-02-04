package dshell


import zzb.shell.{Task, Shell}
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import zzb.config.EnvConfigLoader

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-24
 * Time: 下午8:01
 * Copyright baoxian.com 2012~2020
 */
object ShellApp extends App  with EnvConfigLoader {

  Task.parseConfig(ConfigFactory.load("tasks").getConfig("task"))

  var config = loadConfig("shellRemote").getOrElse(
    throw new Exception("can't found shellRemote.conf")
  )
  var system = ActorSystem("shell-remote", config)
  Shell.init(config,system)

  Shell("demo", "Shell-Demo")

}
