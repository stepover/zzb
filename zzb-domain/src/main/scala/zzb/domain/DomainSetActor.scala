package zzb.domain

import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorRef}
import akka.util.Timeout
import spray.http.StatusCodes._
import zzb.datatype.{TString, DataType}
import zzb.domain.directive.{AuthorizedOperator, AuthorizeDirectives}
import zzb.rest._
import zzb.rest.util.{StatableActor, IdleReleasable}
import zzb.storage.TStorable

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
 * Created by Simon on 2014/7/18
 */
trait DomainSetActor[K, KT <: DataType[K], T <: TStorable[K, KT]] extends RestServiceActor with AuthorizeDirectives with DomainLogging {

  private val hlog = HeritLog("dst")

  def makeChildActor(id: Option[String], isNewCreate: Boolean, data: Option[Any]): ActorRef

  def childByName(id: String): Future[Either[(StatusCode, String), ActorRef]] = {
    import zzb.domain.DomainActor._
    val p = Promise[Either[(StatusCode, String), ActorRef]]()

    //todo:处理 DomainSetActor 创建子actor 超时
    implicit val timeout = Timeout(2000.millis)
    import context.dispatcher

    context.child(id) match {
      case Some(subAct) =>
        hlog(log.debug("found child '{}'", id))
        p.success(Right(subAct))
      case None =>
        hlog(log.debug("start load  child '{}' ...", id))
        try {
          val subAct = makeChildActor(Some(id), isNewCreate = false, None)
          subAct.startInit[T].onComplete {
            case scala.util.Success(InitDocOver(Right(doc))) => p.success(Right(subAct))
            case scala.util.Success(InitDocOver(Left(e@RestException(_)))) =>
              p.success(Left(e.err, e.err.reason))
            case scala.util.Success(InitDocOver(Left(e))) =>
              hlog(log.error(e, "load NewDomain failed 1."))
              p.success(Left(InternalServerError, e.getMessage))
            case scala.util.Failure(e) =>
              hlog(log.error(e, "load NewDomain failed 2."))
              p.success(Left(InternalServerError, e.getMessage))
          }
        } catch {
          case e: Throwable =>
            hlog(log.error(e, "load NewDomain failed 3."))
            p.success(Left(InternalServerError, e.getMessage))
        }

    }
    p.future
  }

  def createNewDomainRoute(opt: AuthorizedOperator, data: Any, createDocOverRoute: T#Pack => Route): Route = {
    import context.dispatcher
    import zzb.domain.DomainActor._

    try {

      implicit val timeout = Timeout(5, TimeUnit.SECONDS)
      hlog(log.debug("creating new domain Actor ..."))
      val planeActor = makeChildActor(None, isNewCreate = true, Some(data))
      //context.actorOf(Props(new PlaneActor(id, storage.specific(id), Some(owner), isNewCreate = true) with IdleReleasable with StatableActor), id)

      onSuccess(checkSuccess(planeActor.startInit[T])) {
        case Right(doc) =>
          createDocOverRoute(doc)
        case Left(e@RestException(_)) =>
          complete(e.err, e.err.reason)
        case Left(e) =>
          hlog(log.error(e, "create NewDomain failed."))
          complete(InternalServerError, e.getMessage)
      }
    } catch {
      case e: Throwable =>
        hlog(log.error(e, "create NewDomain failed "))
        complete(InternalServerError, e.getMessage)
    }
  }

}
