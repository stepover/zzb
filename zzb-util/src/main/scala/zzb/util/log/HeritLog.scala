package zzb.util.log

import akka.event._
/**
* Created by Simon on 2014/7/23
*/
case class HeritLog(herit:String){
  def apply(logStatement: ⇒ Unit)(implicit log:LoggingAdapter ):Unit = {
    log match {
      case mdcLog : DiagnosticLoggingAdapter =>
        mdcLog.mdc( mdcLog.mdc ++ Map("herit" -> herit))
        logStatement
      case _ =>
        logStatement
    }
  }
}

case class HeritLogAdapter(herit:String,la: DiagnosticLoggingAdapter) extends LoggingAdapter{

  def isErrorEnabled = la.isErrorEnabled
  def isWarningEnabled = la.isWarningEnabled
  def isInfoEnabled = la.isInfoEnabled
  def isDebugEnabled = la.isDebugEnabled

  protected def notifyError(message: String): Unit = {
    la.mdc( la.mdc ++ Map("herit" -> herit))
    la.error(message)
  }
  protected def notifyError(cause: Throwable, message: String): Unit = {
    la.mdc( la.mdc ++ Map("herit" -> herit))
    la.error(cause, message)
  }
  protected def notifyWarning(message: String): Unit = {
    val mmm = la.mdc
    la.mdc( la.mdc ++ Map("herit" -> herit))
    la.warning(message)
  }
  protected def notifyInfo(message: String): Unit = {
    la.mdc( la.mdc ++ Map("herit" -> herit))
    la.info(message)
  }
  protected def notifyDebug(message: String): Unit = {
    la.mdc( la.mdc ++ Map("herit" -> herit))
    la.debug(message)
  }

}