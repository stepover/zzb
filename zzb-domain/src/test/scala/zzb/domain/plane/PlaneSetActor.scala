package zzb.domain.plane


import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import zzb.datatype.TString
import zzb.domain.{GetFSMData, DomainSetActor}
import zzb.rest._
import zzb.rest.util.{IdleReleasable, StatableActor}
import zzb.storage._
import akka.pattern._
import scala.concurrent.duration._

import scala.concurrent.Future

class PlaneSetActor extends DomainSetActor[String, TString, Plane.type] {
  override def receive: Receive = runRoute(route)

  def logName: String = "zzb.domain.Plane"


  val storage: Storage[String, TString, Plane.type] =
    memoryStorage[String, TString, Plane.type](Plane)

  val idGenerator = Iterator from 10000

  def initOKRoute(doc: Plane.Pack) = complete(TString(doc.id))

  import context.dispatcher

  implicit val timeout = Timeout(1000.millis)

  def route: Route =
    operator {
      opt =>
        pathEndOrSingleSlash {
          post {
            entity(as[TString#Pack]) {
              owner =>
                createNewDomainRoute(opt, owner.toString(), initOKRoute)
            }
          }
        } ~ pathPrefix("list") {
          get {
            val s = Future.sequence(context.children.map(_.ask(GetFSMData).asInstanceOf[Future[Plane.Pack]]).toList)
            onSuccess(s){
              case planes => complete(planes)
            }
          }

        } ~ forwardChildFuture(childByName, context)
    }

  override def makeChildActor(id: Option[String], isNewCreate: Boolean, data: Option[Any]): ActorRef = id match {
    case None =>
      val newId = idGenerator.next().toString
      data match {
        case Some(owner: String) =>
          context.actorOf(Props(new PlaneActor(newId, storage.specific(newId), Some(owner), isNewCreate = true) with IdleReleasable with StatableActor), newId)
        case _ => throw new IllegalArgumentException
      }

    case Some(theId) =>
      context.actorOf(Props(new PlaneActor(theId, storage.specific(theId), None, isNewCreate = false) with
        IdleReleasable with StatableActor), theId)
  }
}