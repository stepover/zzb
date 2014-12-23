package zzb.srvdemo.akka_rest

import zzb.rest._
import akka.actor.{Props, ActorRef}

/**
 * Created by Simon on 2014/3/23
 */
class DemoRestActor extends RestServiceActor{
  override def receive: Receive = runRoute(route)

  implicit def  childByName(name:String)  = {
    if(name == "r")
      Right(context.actorOf(Props[UserRestActor],"r"))
    else
      Left((StatusCodes.NotFound ,"error"))
  }

  def route:Route = forwardChild
}
