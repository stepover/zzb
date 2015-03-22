package zzb.rest

import akka.actor.{ Actor, ActorLogging, PoisonPill }
import akka.util.Timeout
import zzb.util.MdcLoggingContext
import zzb.util.log.HeritLogAdapter

/**
 * Created by Simon on 2014/7/25
 */
trait ResponderActor extends Actor with ActorLogging {

  def timeout: Option[Timeout] = None

  def requestId = ""

  def requestHeaders: List[RestHeader] = Nil

  private var response_ : Option[RestResponse] = None

  def response = response_

  import context.dispatcher

  if (timeout.isDefined)
    context.system.scheduler.scheduleOnce(timeout.get.duration, self, RequesterTimeout)

  def onTimeout() = {
    log.error("request {} time out after {} ms", requestId, timeout.get.duration)
  }

  def onFailed(e: Throwable) = {}

  def onResponse()

  override def receive: Receive = {
    case r: RestResponse ⇒
      response_ = Some(r)
      onResponse()
      self ! PoisonPill
    case RequesterTimeout ⇒
      onTimeout()
      self ! PoisonPill
    case e: Throwable ⇒
      onFailed(e)
      self ! PoisonPill
    case r ⇒
      onFailed(WrongResponseTypeException(r))
      self ! PoisonPill
  }
}

trait ResponderBase extends ResponderActor {

  type ResponseFunction = RestResponse ⇒ Unit

  type StatusCodesHandler = (Range, ResponseFunction)

  private var statusCodeHandlers: List[StatusCodesHandler] = Nil

  def status(codes: StatusCode*)(handler: ResponseFunction): Unit = {
    codes.foreach { code ⇒
      status(Range(code.intValue, code.intValue + 1))(handler)
    }
  }

  def status(range: Range)(handler: ResponseFunction): Unit =
    statusCodeHandlers :+= (range, handler)

  override def onResponse() = {
    val res = response.get
    statusCodeHandlers.foreach {
      case (c, h) ⇒
        if (c.contains(res.status.intValue)) h(res)
    }
  }

  status(400 to 426) { (res: RestResponse) ⇒
    val headInfo = requestHeaders.map(h ⇒ s"${h.name} = ${h.value}")
    log.warning("wrong request  '{}' ->  {},headers:{} ", requestId, res.status, headInfo)
  }

  status(500 to 510) { res ⇒
    val headInfo = requestHeaders.map(h ⇒ s"${h.name} = ${h.value}")
    log.error("server error  '{}' ->  {} ,headers:{}", requestId, res.status, headInfo)
  }
}

import scala.reflect.ClassTag

abstract class GenResponder[T](implicit mdcLog: MdcLoggingContext, m: ClassTag[T], trans: PartialFunction[Any, T] = null) extends ResponderBase {

  import zzb.rest.RestEntity._
  import zzb.rest.RestResponse._
  import zzb.rest.StatusCodes._

  override def log = HeritLogAdapter("rest", mdcLog)

  status(OK) { res ⇒
    val headInfo = requestHeaders.map(h ⇒ s"${h.name} = ${h.value}")
    log.info("ok request  '{}' ->  {},headers:{} ", requestId, res.status, headInfo)
    res.entity match {
      case Empty ⇒ onEntity(None)

      case NonEmpty(v) ⇒
        try {
          onEntity(Some(to[T](res)))
        } catch {
          case e: ResponseWrongType ⇒
            onWrongType(v)
        }
    }
  }

  def onEntity(v: Option[T])

  def onWrongType(v: Any) =
    log.error("request '{}' ->  {} ,response type mismatch,expect {} ", requestId, v, m)
}

abstract class NoEntityResponder(implicit mdcLog: MdcLoggingContext) extends GenResponder[Nothing] {

  override def onEntity(v: Option[Nothing]): Unit = v match {
    case None ⇒ onSuccess()
    case Some(value) ⇒
      onSuccess()
      log.warning("request '{}' -> response should not has data {}", value)
  }

  def onSuccess()
}

abstract class EntityResponder[T](implicit mdcLog: MdcLoggingContext, m: ClassTag[T], trans: PartialFunction[Any, T] = null) extends GenResponder[T] {

  override def onEntity(v: Option[T]): Unit = v match {
    case Some(value) ⇒ onSuccess(value)
    case None        ⇒ onSuccess()
  }

  def onSuccess() =
    log.warning("request '{}' -> response has not data ", requestId)

  def onSuccess(value: T)
}

