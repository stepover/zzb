package zzb.rest.testkit

import com.typesafe.config.{ ConfigFactory, Config }
import scala.util.DynamicVariable
import scala.util.control.NonFatal
import scala.reflect.ClassTag
import org.scalatest.Suite
import akka.actor.ActorSystem
import zzb.rest.directives.ExecutionDirectives
import zzb.rest.unmarshalling._
import spray.util._
import zzb.rest._

trait RouteTest extends RequestBuilding with RouteResultComponent {
  this: TestFrameworkInterface ⇒

  def testConfigSource: String = ""
  def testConfig: Config = {
    val source = testConfigSource
    val config = if (source.isEmpty) ConfigFactory.empty() else ConfigFactory.parseString(source)
    config.withFallback(ConfigFactory.load())
  }
  implicit val system = ActorSystem(Utils.actorSystemNameFrom(getClass), testConfig)
  implicit def executor = system.dispatcher

  def cleanUp(): Unit = { system.shutdown() }

  private val dynRR = new DynamicVariable[RouteResult](null)

  private def assertInCheck(): Unit = {
    if (dynRR.value == null) sys.error("This value is only available inside of a `check` construct!")
  }

  def check[T](body: ⇒ T): RouteResult ⇒ T = result ⇒ dynRR.withValue(result.awaitResult)(body)

  private def result = { assertInCheck(); dynRR.value }
  def handled: Boolean = result.handled
  def response: RestResponse = result.response
  def entity: RestEntity = response.entity
  @deprecated("Use `responseAs` instead.", "1.0/1.1/1.2-RC1")
  def entityAs[T: Unmarshaller: ClassTag]: T = entity.as[T].fold(error ⇒ failTest("Could not unmarshal response " +
    s"to type '${implicitly[ClassTag[T]]}' for `entityAs` assertion: $error\n\nResponse was: $response"), identityFunc)
  def responseAs[T: FromResponseUnmarshaller: ClassTag]: T = response.as[T].fold(error ⇒ failTest("Could not unmarshal response " +
    s"to type '${implicitly[ClassTag[T]]}' for `responseAs` assertion: $error\n\nResponse was: $response"), identityFunc)
  def body: RestEntity.NonEmpty = entity.toOption getOrElse failTest("Response has no body")
  //def contentType: ContentType = body.contentType
  //def mediaType: MediaType = contentType.mediaType
  //def charset: HttpCharset = contentType.charset
  //def definedCharset: Option[HttpCharset] = contentType.definedCharset
  def headers: List[RestHeader] = response.headers
  def header[T <: RestHeader: ClassTag]: Option[T] = response.header[T]
  def header(name: String): Option[RestHeader] = response.headers.find(_.is(name.toLowerCase))
  def status: StatusCode = response.status
  //def chunks: List[MessageChunk] = result.chunks
  //def closingExtension: String = result.closingExtension
  //def trailer: List[RestHeader] = result.trailer
  def rejections: List[Rejection] = RejectionHandler.applyTransformations(result.rejections)
  def rejection: Rejection = {
    val r = rejections
    if (r.size == 1) r.head else failTest("Expected a single rejection but got %s (%s)".format(r.size, r))
  }

  /**
   * A dummy that can be used as `~> runRoute` to run the route but without blocking for the result.
   * The result of the pipeline is the result that can later be checked with `check`. See the
   * "separate running route from checking" example from ScalatestRouteTestSpec.scala.
   */
  def runRoute: RouteResult ⇒ RouteResult = identity

  // there is already an implicit class WithTransformation in scope (inherited from spray.httpx.TransformerPipelineSupport)
  // however, this one takes precedence
  implicit class WithTransformation2(request: RestRequest) {
    def ~>[A, B](f: A ⇒ B)(implicit ta: TildeArrow[A, B]): ta.Out = ta(request, f)
  }

  abstract class TildeArrow[A, B] {
    type Out
    def apply(request: RestRequest, f: A ⇒ B): Out
  }

  case class DefaultHostInfo(host: RestHeaders.Host, securedConnection: Boolean)
  object DefaultHostInfo {
    implicit def defaultHost: DefaultHostInfo =
      DefaultHostInfo(RestHeaders.Host("example.com"), securedConnection = false)
  }
  object TildeArrow {
    implicit object InjectIntoRequestTransformer extends TildeArrow[RestRequest, RestRequest] {
      type Out = RestRequest
      def apply(request: RestRequest, f: RestRequest ⇒ RestRequest) = f(request)
    }
    implicit def injectIntoRoute(implicit timeout: RouteTestTimeout,
                                 log: LoggingContext, eh: ExceptionHandler, defaultHostInfo: DefaultHostInfo) =
      new TildeArrow[RestReqContext, Unit] {
        type Out = RouteResult
        def apply(request: RestRequest, route: Route) = {
          val routeResult = new RouteResult(timeout.duration)

          val effectiveRequest =
            request.withEffectiveUri(
              securedConnection = defaultHostInfo.securedConnection,
              defaultHostHeader = defaultHostInfo.host)

          ExecutionDirectives.handleExceptions(eh orElse ExceptionHandler.default)(route) {
            RestReqContext(
              request = effectiveRequest,
              responder = routeResult.handler,
              unmatchedPath = effectiveRequest.uri.path)
          }
          routeResult
        }
      }
  }
}

trait ScalatestRouteTest extends RouteTest with ScalatestInterface { this: Suite ⇒ }

trait Specs2RouteTest extends RouteTest with Specs2Interface