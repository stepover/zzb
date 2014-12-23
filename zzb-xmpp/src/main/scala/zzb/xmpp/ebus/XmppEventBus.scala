package zzb.xmpp.ebus

import java.util.concurrent.ConcurrentHashMap
import akka.actor.ActorRef
import akka.event.{EventBus, SubchannelClassification}
import akka.util.Subclassification
import org.jivesoftware.smack.Chat
import org.jivesoftware.smack.packet.Message
import zzb.xmpp.XPresence

/**
 * Created by Simon on 2014/8/8
 */

object XmppEventBus {

  private val xmppEventBus = new XmppEventBus

  implicit def default = xmppEventBus

  def apply(name: String): XmppEventBus =
    if (namedBus.contains(name)) namedBus.get(name)
    else {
      val ebus = new XmppEventBus
      namedBus.put(name, ebus)
      ebus
    }

  private val namedBus = new ConcurrentHashMap[String, XmppEventBus]()
}

/**
 * Xmpp 消息的事件总线
 */
class XmppEventBus private[ebus] extends EventBus with SubchannelClassification {
  type Event = BusEvent
  type Classifier = String
  type Subscriber = ActorRef

  override protected implicit def subclassification: Subclassification[Classifier] = new PathSubclassification

  override protected def publish(event: Event, subscriber: Subscriber): Unit = subscriber ! event

  override protected def classify(event: Event): Classifier = event.path
}


class PathSubclassification extends Subclassification[String] {
  override def isEqual(x: String, y: String): Boolean =
    x == y

  override def isSubclass(x: String, y: String): Boolean =
    x.startsWith(y) || {
      val pattern = y.r
      pattern.findFirstIn(x) match {
        case Some(v) if v == x => true
        case _ => false
      }
    }
}


trait BusEvent {
  val path: String
}

case class XStatus(path: String, jid: String, status: XPresence) extends BusEvent

case class XMessageIn(path: String, from: String, to: String, body: String, subject: String = null, thread: String = null) extends BusEvent

private[xmpp] case class XMessageOut(from: String, to: String, body: String, subject: String = null, thread: String = null)

private[xmpp] case class XRoomPrivateOut(from: String, to: String, body: String, subject: String = null, thread: String = null)

private[xmpp] case class ChatIn(chat: Chat, msg: Message)

