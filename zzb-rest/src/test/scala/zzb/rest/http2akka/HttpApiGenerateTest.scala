package zzb.rest.http2akka

import org.scalatest.{ FreeSpec, BeforeAndAfterAll, MustMatchers }
import akka.util.Timeout
import scala.concurrent.duration._
import spray.testkit.ScalatestRouteTest
import spray.routing.Directives
import spray.http.HttpRequest
import spray.http.StatusCodes
import StatusCodes._

/**
 * Created by Simon on 2014/6/10
 */
class HttpApiGenerateTest extends FreeSpec with MustMatchers with Directives with Http2AkkaDirectives with ScalatestRouteTest
  with BeforeAndAfterAll {
  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

  implicit val timeout = Timeout(5 seconds)

  val api = HttpApi[SomeApi]
  def doReq(request: HttpRequest) = (request ~> api).asInstanceOf[RouteResult]

  "ddd" - {
    "good" in {

      doReq(Get("/ok")) ~> check {
        status mustBe OK
      }
      doReq(Get("/404")) ~> check {
        status mustBe NotFound
      }
    }
  }

}

trait SomeApi extends HttpApi {

  def logName = "zzb.test.some"

  def api = get {
    pathPrefix(Segment) {
      case "ok"  ⇒ complete("ok")
      case "404" ⇒ complete(NotFound)

    }
  }
}