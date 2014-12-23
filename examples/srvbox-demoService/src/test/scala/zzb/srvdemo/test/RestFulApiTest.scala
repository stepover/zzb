package zzb.srvdemo.test

/**
 * User: blackangel
 * Date: 13-9-16
 * Time: 下午4:29
 */
import spray.json._
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import org.scalatest.MustMatchers
import com.typesafe.config.ConfigFactory
import zzb.srvdemo.entites.User
import zzb.srvdemo.DemoService
import akka.actor.{Props, ActorSystem}
import spray.can.Http.Bind
import spray.can.Http
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.HttpRequest
import scala.util.Success
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport._
/*
class RestFulApiTest extends WordSpec with MustMatchers
  with BeforeAndAfterAll with DefaultJsonProtocol{

  val config = ConfigFactory.load("demo")
  implicit val system = ActorSystem("demoApp", config)
  import system.dispatcher

  val service = new DemoService(system,config)
  val host = config.getString("http.host")
  val port = config.getInt("http.port")

  "http config file " must {
    "has db config block" in {
      config.hasPath("http") must equal(true)
      config.hasPath("http.host") must equal(true)
      config.hasPath("http.port") must equal(true)
    }
  }

  import spray.json._
  import User._
  val pipeline: HttpRequest => Future[User] = (
    addHeader("content-type", "application/json")
      ~> sendReceive
      ~> unmarshal[User]
    )
  "http api do " must {
    "user add" in {
      val response: Future[User] =
        pipeline(Post("http://localhost:5000/user", new User("121@1.com","1212")))
      response onComplete {
        case Success(user) =>
          user.email must equal("121@1.com")
      }
    }
    "user list" in {
      val pipeline2: HttpRequest => Future[List[User]] = (
        addHeader("content-type", "application/json")
          ~> sendReceive
          ~> unmarshal[List[User]]
        )
      val response2: Future[List[User]] =
        pipeline2(Get("http://localhost:5000/user"))
      response2 onComplete {
        case Success(users) =>
          users.size must equal(0)
        case e =>
          println("List:"+e)
      }
    }
  }

  override def beforeAll = {
    service.startup("demo",false)
    val api = system.actorOf(Props(new RestInterface()), "httpInterface")
    Http(system).manager ! Bind(listener = api,interface = host,port = port)
  }

  override def afterAll() = {
  }
}
*/