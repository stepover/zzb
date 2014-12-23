package demo.sb

import akka.actor.{ Props, Actor, ActorSystem }
import com.typesafe.config.Config
import zzb.service.{ RestSupport, BoxedService }
import spray.routing._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-27
 * Time: 下午10:16
 * Copyright baoxian.com 2012~2020
 */
class ServiceB(system: ActorSystem, config: Config)
    extends BoxedService(system, config) with RestSupport {
  def fini() {}

  def init() {
    val someActor = system.actorOf(Props[SomeActor], "sb.someActor")

  }

  def routes: Route =
    path("hello") {
      get {
        ctx ⇒
          ctx.complete("greeting from service B!")
      }
    }
}

class SomeActor extends Actor {
  def receive: Actor.Receive = {
    case "hello" ⇒ sender ! "greeting from service B!"
  }
}