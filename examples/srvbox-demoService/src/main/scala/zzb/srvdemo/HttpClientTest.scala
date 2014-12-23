package zzb.srvdemo

import zzb.srvdemo.entites.User
import javax.net.ssl.{TrustManagerFactory, KeyManagerFactory, SSLContext}
import java.security.{SecureRandom, KeyStore}
import scala.concurrent.{Await, Future}
import spray.client.pipelining._

import spray.client.pipelining._
import spray.http.HttpRequest
import scala.util.{Failure, Success}
import spray.httpx.SprayJsonSupport._
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import spray.can.Http
import akka.io.IO
import scala.concurrent.duration._
import akka.util.Timeout
/**
 * Created by blackangel on 13-12-24.
 */
object HttpClientTest extends App{
  val config = ConfigFactory.load("demo")
  implicit val system = ActorSystem("demoApp", config)
  implicit val hostConnectionTimeout = Timeout(60.second)
  import system.dispatcher
  def test{
    import spray.json._
    import User._
    import spray.io.ClientSSLEngineProvider
    import akka.pattern._

    implicit def sslContext: SSLContext = {
      val password = "changeit"
      val keyStore = KeyStore.getInstance("jks")
      val ksIs = User.getClass.getClassLoader.getResourceAsStream("client.jks")
      keyStore.load(ksIs, password.toCharArray)
      val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
      keyManagerFactory.init(keyStore, password.toCharArray)
      val trustManagerFactory = TrustManagerFactory.getInstance("SunX509")
      trustManagerFactory.init(keyStore)
      val context = SSLContext.getInstance("TLS")
      context.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom)
      context
    }
    implicit val myEngineProvider = ClientSSLEngineProvider { engine =>
      engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_128_CBC_SHA"))
      engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))
      engine
    }
    val connectorSetup = Http.HostConnectorSetup("localhost", 5001, sslEncryption=true)
    val hostConnection = Await.result(
      for (Http.HostConnectorInfo(connector, _) <- IO(Http) ? connectorSetup)
      yield {
        sendReceive(connector)
      },Duration.Inf)

    val pipeline: HttpRequest => Future[User] = (
      hostConnection
        ~> unmarshal[User]
      )
    val response: Future[User] =
      pipeline(Post("https://localhost:5001/demo/user", new User("121@1.com","1212")))
    response onComplete {
      case Success(user) =>
        println(user)
      case Failure(e) => println("error")
    }


    val pipeline2: HttpRequest => Future[List[User]] = (
      hostConnection
        ~> unmarshal[List[User]]
      )
    val response2: Future[List[User]] =
      pipeline2(Get("https://localhost:5001/demo/user"))
    response2 onComplete {
      case Success(users) =>
        users.foreach {
          user: User => println(user.email)
        }
        println("List:" + users)
      case e =>
        println("List:" + e)
    }
  }
  test

}
