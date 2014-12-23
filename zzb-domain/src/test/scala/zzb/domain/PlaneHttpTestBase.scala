package zzb.domain

import akka.actor.{ActorSystem, Props}
import akka.event.{BusLogging, DiagnosticLoggingAdapter, LogSource}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigValueFactory}
import org.scalatest.{BeforeAndAfterAll, FreeSpec, MustMatchers}
import spray.http.HttpCharsets._
import spray.http.HttpHeaders.RawHeader
import spray.http.MediaTypes._
import spray.http.{ContentType, HttpEntity, HttpRequest}
import spray.routing.Directives
import spray.testkit.ScalatestRouteTest
import zzb.domain.plane.{PlaneHttpApi, PlaneSvcActor}
import zzb.rest.http2akka.HttpApi
import zzb.rest.util.StatableActor
import zzb.util.MdcLoggingContext

import scala.concurrent.duration._

/**
 * Created by Simon on 2014/7/10
 */
trait PlaneHttpTestBase extends FreeSpec with Directives with MustMatchers with ScalatestRouteTest with BeforeAndAfterAll {

  override protected def createActorSystem(): ActorSystem =
    ActorSystem("testSystem", testConfig)
  override def testConfigSource: String =
    """
      |akka {
      |  loggers = ["akka.event.slf4j.Slf4jLogger"]
      |  loglevel = "warning"
      |}
    """.stripMargin

  override def testConfig: Config = {
    super.testConfig.withValue("akka.log-dead-letters-during-shutdown", ConfigValueFactory.fromAnyRef("off"))
  }

  implicit val log = logFactory("zzb.test")

  implicit val mdcLogCtx = MdcLoggingContext.fromAdapter(log)

  log.mdc(Map("pid" -> 200))

  log.info("start test 111 ")

  log.info("start test 222 ")

  protected def logFactory(logName: String): DiagnosticLoggingAdapter = {

    val (str, clazz) = LogSource(logName)
    new BusLogging(system.eventStream, str, clazz) with DiagnosticLoggingAdapter
  }

  override protected def beforeAll() {
    system.actorOf(Props(new PlaneSvcActor with StatableActor), "api")
  }

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(950 milliseconds)

  val api = pathPrefix("api") {
    pathPrefix(Segment) {
      case "planes" => HttpApi[PlaneHttpApi]
    }
  }

  def user(request: HttpRequest, uname: String = "simon") = (request.withHeaders(RawHeader("X-User-Id", uname)) ~> api).asInstanceOf[RouteResult]

  def manager(request: HttpRequest, uname: String = "adminmmm") = (request.withHeaders(RawHeader("X-Manager-Id", uname)) ~> api).asInstanceOf[RouteResult]

  def entity(jsonStr: String) = HttpEntity(ContentType(`application/json`, `UTF-8`), jsonStr)

  var pid: String = ""
  var alterSeq: Int = -1

}
