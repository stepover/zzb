package zzb.srvbox

import akka.actor.{ActorRef, ActorSystem, ActorLogging, Actor}
import spray.http.StatusCodes
import zzb.srvbox.SrvManageProtocol._
import zzb.service.{RestSupport, BoxedService}
import com.typesafe.config.{ConfigValueFactory, Config}
import spray.routing.RequestContext
import zzb.config.EnvConfigLoader

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-29
 * Time: 下午6:31
 * Copyright goodsl.org 2012~2020
 */
class ServiceActor extends Actor with ActorLogging with EnvConfigLoader {

  var service: BoxedService = null
  var className: String = null
  var sharedActorSystem = true
  var name: String = null
  var restSupport: RestSupport = null

  private def useSystem(config: Config) = {
    if (sharedActorSystem) context.system
    else {
      ActorSystem(name, config)
    }
  }

  var starterRequest = List[ActorRef]()

  def receive = {
    case Register(serviceName, serviceClassName, isSharedActorSystem) ⇒
      this.className = serviceClassName
      this.name = serviceName
      this.sharedActorSystem = isSharedActorSystem

    case "start" ⇒
      if (service == null) {
        val config = loadConfig(name).get.withValue("box.service-name", ConfigValueFactory.fromAnyRef(name))
        val theSystem = useSystem(config)
        service = Class.forName(className).getConstructor(
          classOf[ActorSystem], classOf[Config]).newInstance(theSystem, config).asInstanceOf[BoxedService]

        service.startup(name, sharedActorSystem)

        service match {
          case rs: RestSupport ⇒ this.restSupport = rs
          case _ ⇒ this.restSupport = null
        }
        starterRequest :+= sender
      }else
       sender ! ServiceStatus(name,  running = false)
    case ServiceReady(serviceName) if serviceName == name =>
      starterRequest.foreach(s => s ! ServiceStatus(name,  service != null))

    case "stop" ⇒
      if (service != null) {
        service.shutdown()
        service = null
      }
      sender ! ServiceStatus(name, service != null)


    case "query" ⇒
      sender ! ServiceStatus(name, service != null)

    case ctx: RequestContext ⇒
      if (restSupport != null){
        try {
          restSupport.RestRequest(ctx)
        }catch{
          case e : Throwable => ctx.complete(StatusCodes.InternalServerError,e.getMessage)
        }
      } else
        ctx.complete("No Support")
  }

  override def preRestart(reason: Throwable, message: Option[Any]) {
    self ! Register(name, className, sharedActorSystem)
  }

}
