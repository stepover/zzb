package zzb.domain.plane

import zzb.rest._
import akka.actor.Props
import zzb.rest.util.StatableActor
import spray.http.StatusCodes._

/**
 * Created by Simon on 2014/6/13
 */

class PlaneSvcActor extends RestServiceActor {
  override def receive: Receive = runRoute(route)


  implicit def childByName(name: String) = {
    name match {
      case "planes" =>
        Right(context.actorOf(Props(new PlaneSetActor with StatableActor), "planes"))
      case _ => Left((NotFound, "error"))
    }
  }

  def route: Route =
    pathEndOrSingleSlash {
      post {
        complete("ok")
      }
    } ~
      forwardChild
}
