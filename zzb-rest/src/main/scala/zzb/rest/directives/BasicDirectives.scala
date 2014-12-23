package zzb.rest
package directives

import shapeless._

trait BasicDirectives {

  def mapInnerRoute(f: Route ⇒ Route): Directive0 = new Directive0 {
    def happly(inner: HNil ⇒ Route) = f(inner(HNil))
  }

  def mapRequestContext(f: RestReqContext ⇒ RestReqContext): Directive0 =
    mapInnerRoute { inner ⇒ ctx ⇒ inner(f(ctx)) }

  def mapRequest(f: RestRequest ⇒ RestRequest): Directive0 =
    mapRequestContext(_.withRequestMapped(f))

  def routeRouteResponse(f: PartialFunction[Any, Route]): Directive0 =
    mapRequestContext(_.withRouteResponseRouting(f))

  def mapRouteResponse(f: Any ⇒ Any): Directive0 =
    mapRequestContext(_.withRouteResponseMapped(f))

  def mapRouteResponsePF(f: PartialFunction[Any, Any]): Directive0 =
    mapRequestContext(_.withRouteResponseMappedPF(f))

  def mapRejections(f: List[Rejection] ⇒ List[Rejection]): Directive0 =
    mapRequestContext(_.withRejectionsMapped(f))

  //  def mapHttpResponsePart(f: HttpResponsePart ⇒ HttpResponsePart): Directive0 =
  //    mapRestReqContext(_.withHttpResponsePartMapped(f))

  def mapRestResponse(f: RestResponse ⇒ RestResponse): Directive0 =
    mapRequestContext(_.withHttpResponseMapped(f))

  def mapRestResponseEntity(f: RestEntity ⇒ RestEntity): Directive0 =
    mapRequestContext(_.withHttpResponseEntityMapped(f))

  def mapRestResponseHeaders(f: List[RestHeader] ⇒ List[RestHeader]): Directive0 =
    mapRequestContext(_.withHttpResponseHeadersMapped(f))

  /**
   * A Directive0 that always passes the request on to its inner route
   * (i.e. does nothing with the request or the response).
   */
  def noop: Directive0 = Directive.Empty

  /**
   * A simple alias for the `noop` directive.
   */
  def pass: Directive0 = noop

  /**
   * Injects the given value into a directive.
   */
  def provide[T](value: T): Directive1[T] = hprovide(value :: HNil)

  /**
   * Injects the given values into a directive.
   */
  def hprovide[L <: HList](values: L): Directive[L] = new Directive[L] {
    def happly(f: L ⇒ Route) = f(values)
  }

  /**
   * Extracts a single value using the given function.
   */
  def extract[T](f: RestReqContext ⇒ T): Directive1[T] =
    hextract(ctx ⇒ f(ctx) :: HNil)

  /**
   * Extracts a number of values using the given function.
   */
  def hextract[L <: HList](f: RestReqContext ⇒ L): Directive[L] = new Directive[L] {
    def happly(inner: L ⇒ Route) = ctx ⇒ inner(f(ctx))(ctx)
  }
}

object BasicDirectives extends BasicDirectives