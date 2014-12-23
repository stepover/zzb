package zzb.util

import akka.actor.{ActorContext, ActorSystem, ActorRefFactory}
import akka.event._
import spray.util.LoggingContext

/**
 * Created by Simon on 2014/7/23
 */
trait MdcLoggingContext extends LoggingContext with DiagnosticLoggingAdapter

object MdcLoggingContext extends MdcLoggingContextLowerOrderImplicit1 {
  implicit def fromAdapter(implicit la: DiagnosticLoggingAdapter) = new MdcLoggingContext {
    def isErrorEnabled = la.isErrorEnabled
    def isWarningEnabled = la.isWarningEnabled
    def isInfoEnabled = la.isInfoEnabled
    def isDebugEnabled = la.isDebugEnabled

    override def mdc: MDC = la.mdc
    override def mdc(mdc: MDC): Unit =la.mdc(mdc)

    protected def notifyError(message: String): Unit = la.error(message)
    protected def notifyError(cause: Throwable, message: String): Unit = la.error(cause, message)
    protected def notifyWarning(message: String): Unit = la.warning(message)
    protected def notifyInfo(message: String): Unit = la.info(message)
    protected def notifyDebug(message: String): Unit = la.debug(message)
  }
}


private[util] sealed abstract class MdcLoggingContextLowerOrderImplicit1 extends MdcLoggingContextLowerOrderImplicit2 {
  this: MdcLoggingContext.type ⇒
  implicit def fromActorRefFactory(implicit refFactory: ActorRefFactory) =
    refFactory match {
      case x: ActorSystem  ⇒ fromActorSystem(x)
      case x: ActorContext ⇒ fromActorContext(x)
    }
  def fromActorSystem(system: ActorSystem) = {
    val (str, clazz) = LogSource("system",system)
    val la = new BusLogging(system.eventStream, str, clazz) with DiagnosticLoggingAdapter
    fromAdapter(la)
  }
  def fromActorContext(context: ActorContext) = {
    val (str, clazz) = LogSource(context.self)
    val la = new BusLogging(context.system.eventStream, str, clazz) with DiagnosticLoggingAdapter
    fromAdapter(la)
  }
}

private[util] sealed abstract class MdcLoggingContextLowerOrderImplicit2 {
  this: MdcLoggingContext.type ⇒
  implicit val NoLogging = fromAdapter(MdcNoLogging)
}

object MdcNoLogging extends DiagnosticLoggingAdapter {

  /**
   * Java API to return the reference to NoLogging
   * @return The NoLogging instance
   */
  def getInstance = this

  final override def isErrorEnabled = false
  final override def isWarningEnabled = false
  final override def isInfoEnabled = false
  final override def isDebugEnabled = false

  final protected override def notifyError(message: String): Unit = ()
  final protected override def notifyError(cause: Throwable, message: String): Unit = ()
  final protected override def notifyWarning(message: String): Unit = ()
  final protected override def notifyInfo(message: String): Unit = ()
  final protected override def notifyDebug(message: String): Unit = ()
}
