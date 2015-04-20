package zzb.xmpp

import akka.actor.FSM.Failure
import akka.actor._
import akka.util.Timeout
import org.jivesoftware.smack.SmackException.ConnectionException
import org.jivesoftware.smack._
import org.jivesoftware.smack.filter.{MessageTypeFilter, PacketTypeFilter}
import org.jivesoftware.smack.packet.{Message, Packet, Presence}
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import zzb.xmpp.XUser._
import zzb.xmpp.ebus._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure => FF, Success => SS}

/**
 * Created by Simon on 2014/8/6
 */
class XUser(cfgUser: String, setting: XUserSetting)(implicit bus: XmppEventBus) extends FSM[XPresence, XUserContext] {
  startWith(Offline, XUserContext(new XMPPTCPConnection(setting.config)))

  bus.publish(XStatus(s"/xuser/$cfgUser/status/self", setting.username, Offline))

  bus.subscribe(self, s"/xuser/$cfgUser/chat-to")

  var doReconnect = true

  private def conn = stateData.conn

  //注册连接监听器
  conn.addConnectionListener(new ConnectionListener {
    override def connected(connection: XMPPConnection): Unit = {
      log.debug(s"<$cfgUser> connection listener got connected")
      self ! Connected(cfgUser)
    }

    override def reconnectionFailed(e: Exception): Unit = ()

    override def reconnectionSuccessful(): Unit = {
      log.debug(s"<$cfgUser> connection listener got reconnection successful")
      //重连成功时楼上的 connected 方法也会被回调，这里就不用再发Connected消息了
    }

    override def authenticated(connection: XMPPConnection): Unit = self ! Authenticated

    override def connectionClosedOnError(e: Exception): Unit = self ! ConnectionLost

    override def connectionClosed(): Unit = ConnectionLost

    override def reconnectingIn(seconds: Int): Unit = ()
  })

  //注册好友在线状态变更监听器
  conn.addPacketListener(new PacketListener {
    override def processPacket(packet: Packet): Unit = {
      val jid = packet.getFrom
      val presence = packet.asInstanceOf[Presence]
      bus.publish(XStatus(s"/xuser/$cfgUser/status/friends/$jid", jid, if (presence.isAvailable) Online else Offline))
      log.debug(s"<$cfgUser> presence listener got {} => {}", packet.getFrom, packet.toString)
    }
  }, NotConferencePresenceFilter)

  val chatMessageListener = new MessageListener {
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
      if (!createdLocally && !chat.getParticipant.contains("conference")) {
        //为其他用户创建的 Chat 绑定消息处理器
        chat.addMessageListener(chatMessageListener)
      }
  }

  ChatManager.getInstanceFor(conn).addChatListener(privateChatCreateListener)

  self ! DoLogin

  import context.dispatcher

  when(Offline) {
    case Event(DoLogin, _) =>
      doReconnect = true
      log.debug(s"<$cfgUser> start connecting...")
      Future {
        conn.connect()
      }.onFailure {
        case e: ConnectionException =>
          log.error(s"<$cfgUser> xmpp connect to {} failed ", e.getFailedAddresses)
          self ! ConnectFailed
        case e: Throwable =>
          log.error(e, s"<$cfgUser> connect failed")
          self ! ConnectFailed
      }
      goto(Connecting)

    case Event(Authenticated, _) =>
      log.debug(s"<$cfgUser> login Ok!")
      goto(Online)
  }

  when(Connecting) {
    case Event(Connected(jid), _) =>
      log.debug(s"<$cfgUser> $jid Connected,start login...")
      Future {
        conn.login(setting.username, setting.password, setting.resource)
      }.onFailure {
        case e: Throwable =>
          self ! LoginFailed(e)
      }
      stay()
    case Event(ConnectFailed, _) =>
      context.system.scheduler.scheduleOnce(1000.milli, self, DoLogin)
      goto(Offline)

    case Event(LoginFailed(e), _) =>
      stop(Failure(e))

    case Event(Authenticated, _) =>
      log.debug(s"<$cfgUser> login Ok!")
      goto(Online)

    case Event(DoLogout, _) =>
      doReconnect = false
      goto(Offline)
  }
  when(Online) {
    case Event(XMessageOut(_, to, body, subject, thread), ctx) =>
      log.debug(s"<$cfgUser> want to send msg to {},[{}]:{}", to, subject, body)
      val chatId = if (thread != null && thread.length > 0) s"$to-$thread" else to
      val chat = ctx.chats.getOrElse(chatId, {
        val newChat = ChatManager.getInstanceFor(ctx.conn).createChat(to, thread, chatMessageListener)
        newChat
      })
      val m = new Message()
      m.setBody(body)
      if (subject != null && subject.length > 0) m.setSubject(subject)
      if (thread != null && thread.length > 0) m.setThread(thread)

      chat.sendMessage(m)
      stay() using ctx.copy(chats = ctx.chats + (chatId -> chat))

    case Event(ChatIn(chat, msg), ctx) =>
      val from = msg.getFrom
      var pathStr = s"/xuser/$cfgUser/chat-from/$from"
      val subject = msg.getSubject
      val thread = msg.getThread

      if (thread != null && thread.length > 0) {
        pathStr = pathStr + s"/$thread"
        if (subject != null && subject.length > 0)
          pathStr = pathStr + s"/$subject"
      }

      bus.publish(XMessageIn(pathStr, from, msg.getTo, msg.getBody, msg.getSubject, msg.getThread))

      val chatId = if (thread != null && thread.length > 0) s"$from-$thread" else from
      if (!ctx.chats.contains(chatId))
        stay() using ctx.copy(chats = ctx.chats + (chatId -> chat))
      else
        stay()

    case Event(DoLogout, _) =>
      doReconnect = false
      conn.disconnect()
      goto(Offline)
  }

  when(Offline, Online, Connecting) {
    //只要掉线就重连
    case Event(ConnectionLost, _) =>
      if (doReconnect) {
        log.warning(s"<$cfgUser> connect lost")
        conn.connect()
        goto(Connecting)
      } else goto(Offline)
  }

  whenUnhandled {
    case Event(value, stateData) ⇒
      log.warning(s"<$cfgUser>  unhandled event " + value + " in state " + stateName)
      stay()
  }

  onTransition {
    case from -> to =>
      log.debug(s"<$cfgUser> status from 【{}】 to 【{}】", from, to)
      bus.publish(XStatus(s"/xuser/$cfgUser/status/self", setting.username, to))
  }

  var roomActors = List[ActorRef]()

  //上线后自动进入房间
  onTransition {
    case from -> Online if setting.rooms.nonEmpty =>
      setting.rooms.foreach(roomName => {
        roomActors = context.actorOf(Props(new XRoom(cfgUser, roomName, conn,setting.rejoin)), roomName) :: roomActors
      })
  }

  //离线后销毁所有进入的房间
  onTransition {
    case from -> Offline =>
      roomActors.foreach(r => r ! PoisonPill)
      roomActors = Nil
  }

  def when(stateNames: XPresence*)(stateFunction: StateFunction): Unit =
    stateNames.foreach(s => super.when(s, null)(stateFunction))
}

object XUser {
  def start(cfgUser: String)(implicit settings: XUserSettings, system: ActorSystem) = {

    val xuserRef = system.actorSelection(s"/user/xmpp-$cfgUser")

    implicit val timeout = Timeout(2000.millis)
    import system.dispatcher

    xuserRef.resolveOne().onComplete {
      case SS(ref) => ref ! DoLogin
      case FF(e) ⇒
        val setting = settings.users(cfgUser)
        system.actorOf(Props(new XUser(cfgUser, setting)), s"xmpp-$cfgUser")
    }
  }

  def stop(cfgUser: String)(implicit system: ActorSystem) = {
    val xuser = system.actorSelection(s"/user/xmpp-$cfgUser")
    xuser ! DoLogout
  }

  def startAll(implicit settings: XUserSettings, system: ActorSystem) = {
    settings.users.keySet.foreach(start)
  }

  def apply(cfgUser: String)(implicit system: ActorSystem) = SendProxy(cfgUser)(system)

  case class SendProxy(cfgUser: String)(implicit system: ActorSystem) {
    def sendMsgTo(toJid: String, body: String, subject: String = null, thread: String = null) = {
      val xuser = system.actorSelection(s"/user/xmpp-$cfgUser")
      xuser ! XMessageOut("", toJid, body, subject, thread)
    }
  }

  case class Friend(jid: String, status: XPresence)

  type Friends = Map[String, Friend]
  type Chats = Map[String, Chat]

  object DoLogin

  object DoLogout

  case class Connected(jid: String)

  case object ConnectFailed

  case class LoginFailed(e: Throwable)

  case object ConnectionLost

  case object Authenticated

}

case class XUserContext(conn: XMPPConnection = null, chats: Chats = Map[String, Chat](), friends: Friends = Map[String, Friend]())