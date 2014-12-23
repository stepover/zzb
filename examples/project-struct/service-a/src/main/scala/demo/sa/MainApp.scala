package demo.sa

import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-31
 * Time: 下午8:47
 * Copyright baoxian.com 2012~2020
 */
object MainApp extends App {
  val config = ConfigFactory.load("service-a")

  val system = ActorSystem("demoApp", config)

  val service = new ServiceA(system, config)
  service.startup("service-a", false)

}
