package zzb.util.log

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.AbstractMatcherFilter
import ch.qos.logback.core.spi.FilterReply

import scala.beans.BeanProperty

/**
 * Created by Simon on 2014/6/30
 */
class MDCFilter extends AbstractMatcherFilter[ILoggingEvent] {
  override def decide(event: ILoggingEvent): FilterReply = {
    if (!isStarted) {
      return FilterReply.NEUTRAL
    }
    val mdc = event.getMDCPropertyMap
    if (mdc == null || MDCKey == null || Value == null || !mdc.containsKey(MDCKey)) return FilterReply.NEUTRAL
    if (mdc.get(MDCKey) == Value)  onMatch  else onMismatch
  }

  @BeanProperty
  var Value: String = _

  @BeanProperty
  var MDCKey: String = _


  private var accepts = Set[String]()

  def allowValues_(keys: String) = {
    accepts = keys.split(",").toSet
    println("accepts-->" + accepts)
  }

}
