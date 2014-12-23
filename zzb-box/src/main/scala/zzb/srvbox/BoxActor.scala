package zzb.srvbox

import akka.actor.{ActorRef, Props, ActorLogging, Actor}
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext}
import org.joda.time.DateTime
import scala.util.{Failure, Success}

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-7-27
 * Time: 下午4:59
 * Copyright goodsl.org 2012~2020
 */
class BoxActor extends Actor with ActorLogging {

  import SrvManageProtocol._
  import spray.httpx.SprayJsonSupport._

  implicit val timeout = Timeout(10 minutes)

  import context.dispatcher

  //拦截过滤器
  private val filters:collection.mutable.Map[String,collection.mutable.Map[String,ActorRef]] =collection.mutable.Map.empty

  def receive = {
    case Register(name, className, share) ⇒
      val serviceActor = context.child(name).getOrElse(context.actorOf(Props[ServiceActor], name))
      serviceActor ! Register(name, className, share)

    case RequestStart(name) ⇒

      context.child(name) match {
        case Some(serviceActor) ⇒ serviceActor.tell("start",sender())
        case None ⇒ sender ! ServiceNotExist
      }


    case RequestStop(name) ⇒

      context.child(name) match {
        case Some(serviceActor) ⇒ serviceActor.tell("stop",sender())
        case None ⇒ sender ! ServiceNotExist
      }


    case RequestList ⇒

      val capturedSender = sender
      val futures = context.children.map(serviceActor ⇒ askAndMapToServiceStatus(serviceActor, "query"))

      Future.sequence(futures).map {
        services ⇒
          capturedSender ! Services(services.toList)
      }


    case Halt ⇒
      val futures = context.children.map(serviceActor ⇒ askAndMapToServiceStatus(serviceActor, "stop"))
      val fs = Future.sequence(futures).map {
        services ⇒
          context.system.shutdown()
          true
      }
      fs.onComplete {
        case _ => sysExit()
      }


    case RestRequest(name, ctx) ⇒
      context.child(name) match {
        case Some(serviceActor) ⇒
          filters.get(name) match {
            case Some(filterMap) =>
              Future.sequence(filterMap.map(_._2 ? RestRequest(name, ctx))) onComplete {
                case Success(results) =>
                  //val test=results.filter(_.isInstanceOf[FilterError]).toList
                  results.filter(_.isInstanceOf[FilterError]).toList match {
                    case List() =>serviceActor ! ctx
                    case errors=>
                      ctx.complete(errors.asInstanceOf[List[FilterError]])

                  }

                case Failure(e) => ctx.complete(List(FilterError(e.getMessage)))
              }

            case None =>serviceActor ! ctx
          }

        case None ⇒ ctx.complete(s"Service $name not found")
      }


    case FilterReg(name,serverNames) =>
      log.info("filter {} reg",name)
      serverNames.foreach(serverName=>{
        filters.getOrElseUpdate(serverName,collection.mutable.Map(name->sender))(name)=sender
      })
      sender ! FilterReg(name,serverNames)

    case FilterUnReg(name) =>
      log.info("filter {} cancel",name)
      filters.foreach(_._2.remove(name))
      filters.filter(_._2.size==0).foreach(f=>filters.remove(f._1))
      sender ! FilterUnReg(name)

    case FilterList =>
      sender ! filters.map{case (key,value)=> key->value.map(_._1).mkString(",")}.toMap

  }

  def askAndMapToServiceStatus(serviceActor: ActorRef, cmd: String) = {
    serviceActor.ask(cmd).mapTo[ServiceStatus]
  }


  override def preRestart(reason: Throwable, message: Option[Any]) {
    //过滤器再注册机制
    filters.flatMap(_._2).foreach(f=>f._2 ! FilterUnReg(f._1))

    super.preRestart(reason, message)
  }

  override def postRestart(reason: Throwable) {

    super.postRestart(reason)
  }

  def sysExit() {
    log.info("System Shutdown at {}", DateTime.now().toString("yyyy-MM-dd HH:mm:ss"))
    System.exit(0)
  }
}

