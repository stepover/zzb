package zzb.xmpp

import akka.actor.ActorRefFactory
import com.typesafe.config.{ConfigValue, Config}
import org.jivesoftware.smack.ConnectionConfiguration
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import spray.util._

/**
 * Created by Simon on 2014/8/6
 */

/**
 * 从配置文件中加载指定 Jabber 用户的配置信息
 */

case class XUserSettings(users:Map[String,XUserSetting])

object XUserSettings extends SettingsCompanion[XUserSettings]("zzb.xmpp") {
  override def fromSubConfig(c: Config):XUserSettings ={
    import scala.collection.JavaConverters._

    def userSettingFromConfig(uc:Config):XUserSetting = {
      val host = uc.getString("host")
      val port = uc.getInt("port")
      val xmppConfig = new ConnectionConfiguration(host,port)
      val cc = uc.entrySet().asScala.map(entry => entry.getKey)

      cc.foreach {
        case (key @ "securityMode") => xmppConfig.setSecurityMode(SecurityMode.valueOf(uc.getString(key))  )
        case (key @ "reconnectionAllowed") => xmppConfig.setReconnectionAllowed(uc.getBoolean(key))
        case _ => ()
      }

      val rooms = if(uc.hasPath("rooms")) uc.getStringList("rooms").asScala.toList else List[String]()

      val rejoin = if(uc.hasPath("rejoin")) uc.getBoolean("rejoin") else false

      XUserSetting(uc.getString("username"),uc.getString("password"),uc.getString("resource"),xmppConfig,rooms,rejoin)

    }

    val usersCfg = c.getConfig("users")

    val listedUsers = usersCfg.entrySet().asScala.map(_.getKey.split('.').toList.head)

    val users: Map[String, XUserSetting] = listedUsers.map(u => (u,userSettingFromConfig(usersCfg.getConfig(u)))).toMap


    new XUserSettings(users)
  }



  implicit def default(implicit refFactory: ActorRefFactory) =
    apply(actorSystem)
}

case class XUserSetting(username :String,password:String,resource:String,config:ConnectionConfiguration,rooms:List[String] = Nil,rejoin:Boolean=false)
