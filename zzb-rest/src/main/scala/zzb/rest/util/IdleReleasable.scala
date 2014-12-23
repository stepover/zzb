package zzb.rest.util

import spray.http.DateTime
import akka.actor.{ Actor, PoisonPill }

/**
 * Created by Simon on 2014/3/27
 */
trait IdleReleasable extends Actor {
  this: StatableActor ⇒

  //seconds
  val maxIdle: Int

  def preIdleRelease: Boolean

  val initDelay = maxIdle / 3
  val interval = maxIdle / 3

  import scala.concurrent.duration._
  import context.dispatcher

  val cancelCheck = context.system.scheduler.schedule(initDelay.seconds, interval.seconds, self, RequestStat)

  abstract override def receive: Receive = {
    case MessageStat(start, first, last, count) ⇒
      val lastTime = if (last.isEmpty) start else last.get

      val diff = DateTime.now.clicks - lastTime.clicks

      if (diff > maxIdle * 1000 && preIdleRelease) {
        cancelCheck.cancel()
        self ! PoisonPill

      }

    case msg ⇒ super.receive(msg)
  }
}
