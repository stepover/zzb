package zzb.util.log

import akka.event.LoggingAdapter

/**
 * Created by Simon on 2015/3/31 
 */
object Clock {

  def timeSpan2String(millis:Long):String = {

    val second = millis / 1000
    val ms = millis % 1000

    val msStr = if(ms < 10) "00" + ms  else if (ms < 100) "0" + ms else ms.toString

    second + "." + msStr + " seconds"
  }

  def clocking[T](message: String)( body: => T)(implicit logger : LoggingAdapter):T = {
    logger.info("|<--- " + message)
    doClocking(body)
  }

  def clocking[T](template: String, arg1: Any)( body: => T)(implicit logger : LoggingAdapter):T = {
    logger.info("|<--- " + template,arg1)
    doClocking(body)
  }

  def clocking[T](template: String, arg1: Any, arg2: Any)( body: => T)(implicit logger : LoggingAdapter):T = {
    logger.info("|<--- " + template,arg1,arg2)
    doClocking(body)
  }

  def clocking[T](template: String, arg1: Any, arg2: Any, arg3: Any)( body: => T)(implicit logger : LoggingAdapter):T = {
    logger.info("|<--- " + template,arg1,arg2,arg3)
    doClocking(body)
  }

  def clocking[T](template: String, arg1: Any, arg2: Any, arg3: Any, arg4: Any)( body: => T)(implicit logger : LoggingAdapter):T = {
    logger.info("|<--- " + template,arg1,arg2,arg3,arg4)
    doClocking(body)
  }

  private def doClocking[T]( body: => T)(implicit logger : LoggingAdapter) :T= {
    val beginClock = System.currentTimeMillis()
    val result = body
    val endClock = System.currentTimeMillis()

    logger.info("|<--- Elapsed time: {}",timeSpan2String(endClock - beginClock))

    result
  }
}
