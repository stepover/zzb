package zzb.rest.util

import spray.http.DateTime
import akka.actor.Actor
import spray.json._

/**
 * Created by Simon on 2014/3/25
 */

/**
 * 给Actor 增加消息统计的功能
 */
trait StatableActor extends Actor {
  /**
   *  actor 启动时间
   */
  val startTime = DateTime.now

  /**
   * 收到第一个消息的时间
   */
  var firstMessageTime: Option[DateTime] = None
  /**
   * 最近消息的时间
   */
  var lastMessageTime: Option[DateTime] = None
  /**
   * 消息数量
   */
  var messageCount: Long = 0

  abstract override def receive: Receive = {
    case RequestStat ⇒
      sender ! stat
    case msg: MessageStat ⇒
      super.receive(msg)
    case msg ⇒
      val nowTime = DateTime.now
      if (firstMessageTime.isEmpty) firstMessageTime = Some(nowTime)
      lastMessageTime = Some(nowTime)
      messageCount = messageCount + 1
      super.receive(msg)

  }

  def stat = MessageStat(startTime, firstMessageTime, lastMessageTime, messageCount)

  //def msgReceive: Receive

}

case class MessageStat(start: DateTime, first: Option[DateTime], last: Option[DateTime], count: Long)

object MessageStat extends DefaultJsonProtocol {

  implicit val dateTimeFormat = new RootJsonFormat[DateTime] {
    def write(c: DateTime) =
      JsString(c.toIsoDateTimeString)

    def read(value: JsValue) = DateTime.fromIsoDateTimeString(value.toString()).get

  }

  implicit val statFormat = jsonFormat4(MessageStat.apply)
}

case object RequestStat
