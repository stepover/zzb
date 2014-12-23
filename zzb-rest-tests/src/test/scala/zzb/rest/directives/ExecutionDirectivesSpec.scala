package zzb.rest
package directives

import StatusCodes._

class ExecutionDirectivesSpec extends RoutingSpec {

  "the 'dynamicIf' directive" should {
    "cause its inner route to be revaluated for every request anew, if enabled" in {
      var a = ""
      val staticRoute = get { dynamicIf(enabled = false) { a += "x"; complete(a) } }
      val dynamicRoute = get { dynamic { a += "x"; complete(a) } }
      def expect(route: Route, s: String) = Get() ~> route ~> check { responseAs[String] === s }
      expect(staticRoute, "x")
      expect(staticRoute, "x")
      expect(staticRoute, "x")
      expect(dynamicRoute, "xx")
      expect(dynamicRoute, "xxx")
      expect(dynamicRoute, "xxxx")
    }
  }

  "the 'detach directive" should {
    "handle exceptions thrown inside its inner future" in {

      implicit val exceptionHandler = ExceptionHandler {
        case e: ArithmeticException ⇒ ctx ⇒
          ctx.complete(InternalServerError, "Oops.")
      }

      val route = get {
        detach() {
          complete((3 / 0).toString)
        }
      }

      Get() ~> route ~> check {
        status === InternalServerError
        responseAs[String] === "Oops."
      }
    }
  }
}
