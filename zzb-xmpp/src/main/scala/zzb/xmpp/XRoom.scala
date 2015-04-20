package zzb.xmpp

import akka.actor.{ActorSystem, Actor, ActorLogging}
import org.jivesoftware.smack.packet.{Message, Presence, Packet}
import org.jivesoftware.smack._
import org.jivesoftware.smackx.muc.MultiUserChat
import zzb.xmpp.ebus._
import scala.collection._
import scala.concurrent.Future
import scala.util.Success

/**
 * Created by Simon on 2014/8/12
 */
class XRoom(cfgUser: String, roomName: String, conn: XMPPConnection,rejoin:Boolean=false)(implicit bus: XmppEventBus) extends Actor with ActorLogging {

  //保存参与者的昵称
  val members = mutable.Set[String]()

  val mark = s"<$cfgUser-$roomName>"

  //昵称 -> 对话
  val chats = mutable.Map[String, Chat]()

  log.debug(s"$mark try to join")
  val muc = new MultiUserChat(conn, roomName)
  // muc.addPresenceInterceptor()

  import context.dispatcher

  def notifyEnter(jid: String) = bus.publish(XStatus(s"/xuser/$cfgUser/status/$jid", jid, Online))

  def notifyLeave(jid: String) = bus.publish(XStatus(s"/xuser/$cfgUser/status/$jid", jid, Offline))

  val memberPresenceListener = new PacketListener {
    override def processPacket(packet: Packet): Unit = {
      val fromJid = packet.getFrom
      val presence = packet.asInstanceOf[Presence]
      log.debug(s"$mark got presence {} => {}", fromJid, packet.toString)

      if (presence.isAvailable)
        self ! Enter(fromJid)

      else {
        if(rejoin && fromJid.endsWith(cfgUser))
          self ! JoinRoom
        self ! Leave(fromJid)
      }
    }
  }

  val privateMessageListener = new MessageListener {
    override def processMessage(chat: Chat, message: Message): Unit = {
      if (message.getBody.startsWith("echo ")) {
        val m = new Message()
        m.setBody(message.getBody.substring(5))
        m.setSubject(message.getSubject)
        m.setThread(message.getThread)
        chat.sendMessage(m)
      }
      self ! ChatIn(chat, message)
    }
  }

  val privateChatCreateListener = new ChatManagerListener {
    override def chatCreated(chat: Chat, createdLocally: Boolean): Unit =
      if (!createdLocally && chat.getParticipant.startsWith(roomName)) {
        //为其他用户创建的 Chat 绑定消息处理器
        chat.addMessageListener(privateMessageListener)
      }
  }

  ChatManager.getInstanceFor(conn).addChatListener(privateChatCreateListener)

  conn.addPacketListener(memberPresenceListener, ConferencePresenceFilter(roomName))

  def joinRoom= {
    Future {
      muc.join(cfgUser)
    }.onComplete {
      case Success(v) =>
        log.debug(s"$mark join success")
      //订阅内部系统向外对话的请求，

      case scala.util.Failure(e) =>
        log.error(e, s"$mark join  failed")
    }
  }
  self ! JoinRoom

  override def receive: Receive = {
    case JoinRoom =>
      joinRoom
    case Enter(fromJid) if !members.contains(fromJid) =>
      members.add(fromJid)
      log.debug(s"$mark members {},after add {},", members, fromJid)
      notifyEnter(fromJid)
    case Leave(fromJid) if members.contains(fromJid) =>
      members.remove(fromJid)
      log.debug(s"$mark members {},after remove {},", members, fromJid)
      notifyLeave(fromJid)

    case XRoomPrivateOut(_, toJid, body, subject, thread) =>
      val chatId = if (thread != null && thread.length > 0) s"$toJid-$thread" else toJid
      //私聊
      val chat = chats.getOrElseUpdate(chatId, {
        ChatManager.getInstanceFor(conn).createChat(toJid, thread, privateMessageListener)
        //muc.createPrivateChat(toJid, privateMessageListener)
      })
      log.debug(s"$mark try to send private msg '{}' to {}", body, toJid)

      val m = new Message()
      m.setBody(body)
      if (subject != null && subject.length > 0) m.setSubject(subject)
      if (thread != null && thread.length > 0) m.setThread(thread)

      chat.sendMessage(m)

    case ChatIn(chat, msg) =>
      val fromJid = msg.getFrom
      val thread = msg.getThread
      val subject = msg.getSubject

      val chatId = if (thread != null && thread.length > 0) s"$fromJid-$thread" else fromJid
      if (!chats.contains(chatId)) chats(chatId) = chat
      var pathStr = s"/xuser/$cfgUser/chat-from/$fromJid"

      if (thread != null && thread.length > 0) {
        pathStr = pathStr + s"/$thread"
        if (subject != null && subject.length > 0) {
          pathStr = pathStr + s"/$subject"
        }
      }
      bus.publish(XMessageIn(pathStr, fromJid, msg.getTo, msg.getBody, msg.getSubject, msg.getThread))
  }

  override def postStop() = {
    members.foreach(notifyLeave)
    members.clear()
    conn.removePacketListener(memberPresenceListener)
    chats.values.foreach(_.close())
    ChatManager.getInstanceFor(conn).removeChatListener(privateChatCreateListener)
  }

  private case class Enter(jid: String)

  private case class Leave(jid: String)

  private object JoinRoom

}

object XRoom {

  case class PMSendProxy(cfgUser: String, roomName: String)(implicit system: ActorSystem) {
    def sendMsgTo(toNick: String, body: String, subject: String = null, thread: String = null) = {
      val xroom = system.actorSelection(s"/user/xmpp-$cfgUser/$roomName")
      xroom ! XRoomPrivateOut("", s"$roomName/$toNick", body, subject, thread)
    }
  }

  def apply(cfgUser: String, roomName: String)(implicit system: ActorSystem) = PMSendProxy(cfgUser, roomName)(system)
}
