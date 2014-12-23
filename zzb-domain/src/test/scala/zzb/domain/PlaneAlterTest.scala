package zzb.domain

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpec}
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes._
import spray.http.{StatusCodes, Uri}
import spray.util._
import zzb.datatype._
import zzb.domain.directive.AuthorizedOperator
import zzb.domain.plane.{Plane, PlaneSvcActor}
import zzb.rest._
import zzb.rest.util.StatableActor

import scala.concurrent.duration._

/**
* Created by Simon on 2014/6/19
*/
class PlaneAlterTest extends WordSpec with MustMatchers with Caller with BeforeAndAfterAll {


  protected def createActorSystem(): ActorSystem =
    ActorSystem("testSystem", testConfig)
  def testConfigSource: String =
    """
      |akka {
      |  loggers = ["akka.event.slf4j.Slf4jLogger"]
      |  loglevel = "debug"
      |}
    """.stripMargin


  def testConfig: Config = {
    val source = testConfigSource
    val srcConfig = if (source.isEmpty) ConfigFactory.empty() else ConfigFactory.parseString(source)
    srcConfig.withValue("akka.log-dead-letters-during-shutdown", ConfigValueFactory.fromAnyRef("off"))
  }

  implicit val system = createActorSystem()

  implicit def executor = system.dispatcher


  override protected def beforeAll() {

    system.actorOf(Props(new PlaneSvcActor with StatableActor), "api")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(5000 milliseconds)

  val adminHeader = List(RawHeader("X-Manager-Id", "system"))
  val userHeader = List(RawHeader("X-User-Id", "Simon"))

  val admin = AuthorizedOperator("system",isManager = true)
  val user = AuthorizedOperator("Simon",isManager = false)

  implicit val rootUri = Uri("")

  var pid = ""
  def puri = s"/api/planes/$pid"

  import zzb.domain.AlterSupport._
  import zzb.rest.RestResponse._

  "Alter Support" must {
    "可以创建一个新的领域对象 " in {
      val res = doPost("/api/planes", TString("Simon"), adminHeader).await
      res.status mustBe OK
      pid = res.entity.asString
    }
    "禁止未经认证的用户访问" in {
      try{
        doAlter(puri,"/foods/water", TInt(10)).await
      }catch {
        case AlterFailed(AlterPhase.Request,Forbidden,_) => ()
        case e:Throwable => throw e
      }
    }

    "访问不存在的文档会报 404 " in {
        try{
          doAlter("/api/planes/nothisone","/foods/water", TInt(10),admin).await
        }catch {
          case AlterFailed(AlterPhase.Request,NotFound,_) => ()
          case e:Throwable => throw e
        }
    }
    "可以正确修改数据  " in {
        doAlter(puri, "/foods/water", TInt(10), admin).await
        doGet(puri + "/latest/foods/water").map(to[Int]).await mustBe 10
    }

    "类型不正确会报 400 错误" in {
      try{
        doAlter(puri, "/foods/water", TString("20"), admin).await
      }catch {
        case AlterFailed(AlterPhase.Execute,BadRequest,_) => ()
        case e:Throwable => throw e
      }
    }

    "可以正确修改根数据  " in {
      val plane=doGet(puri + "/latest/").map(to[Plane.Pack]).await
      //val plane=  Plane(Foods(Foods.water := 20))
      doAlter(puri, "/", plane, admin).await
      doGet(puri + "/latest/foods/water").map(to[Int]).await mustBe 10
    }


  }

}
