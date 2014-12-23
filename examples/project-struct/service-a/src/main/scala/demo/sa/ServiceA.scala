package demo.sa

import zzb.service.{ RestSupport, BoxedService }
import akka.actor.{ Props, Actor, ActorSystem }
import com.typesafe.config.Config
import spray.routing._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-27
 * Time: 下午10:13
 * Copyright baoxian.com 2012~2020
 */
class ServiceA(system: ActorSystem, config: Config)
    extends BoxedService(system, config) with RestSupport {
  def fini() {}

  def init() {

    val someActor = system.actorOf(Props[SomeActor], "sa.someActor")

    //测试 deplib 目录是否进入了classpath
    Class.forName("org.h2.Driver")
  }

  def routes: Route =
    path("hello") {
      get {
        ctx ⇒
          ctx.complete("greeting from service A! ")
      }
    }
}

class SomeActor extends Actor {
  def receive: Actor.Receive = {
    case "hello" ⇒ sender ! "greeting from service A!"
  }
}