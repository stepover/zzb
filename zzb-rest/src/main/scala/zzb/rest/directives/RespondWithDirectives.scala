package zzb.rest

package directives

import shapeless.HNil

trait RespondWithDirectives {
  import BasicDirectives._
  import RouteDirectives._

  /**
   * Overrides the given response status on all HTTP responses of its inner Route.
   */
  def respondWithStatus(responseStatus: StatusCode): Directive0 =
    mapRestResponse(_.copy(status = responseStatus))

  /**
   * Unconditionally adds the given response header to all HTTP responses of its inner Route.
   */
  def respondWithHeader(responseHeader: RestHeader): Directive0 =
    mapRestResponseHeaders(responseHeader :: _)

  /**
   * Adds the given response header to all HTTP responses of its inner Route,
   * if the response from the inner Route doesn't already contain a header with the same name.
   */
  def respondWithSingletonHeader(responseHeader: RestHeader): Directive0 =
    mapRestResponseHeaders { headers ⇒
      if (headers.exists(_.is(responseHeader.lowercaseName))) headers
      else responseHeader :: headers
    }

  /**
   * Unconditionally adds the given response headers to all HTTP responses of its inner Route.
   */
  def respondWithHeaders(responseHeaders: RestHeader*): Directive0 =
    respondWithHeaders(responseHeaders.toList)

  /**
   * Unconditionally adds the given response headers to all HTTP responses of its inner Route.
   */
  def respondWithHeaders(responseHeaders: List[RestHeader]): Directive0 =
    mapRestResponseHeaders(responseHeaders ::: _)

  /**
   * Adds the given response headers to all HTTP responses of its inner Route,
   * if a header already exists it is not added again.
   */
  def respondWithSingletonHeaders(responseHeaders: RestHeader*): Directive0 =
    respondWithSingletonHeaders(responseHeaders.toList)

  /* Adds the given response headers to all HTTP responses of its inner Route,
   * if a header already exists it is not added again.
   */
  def respondWithSingletonHeaders(responseHeaders: List[RestHeader]): Directive0 =
    mapRestResponseHeaders {
      responseHeaders.foldLeft(_) {
        case (headers, h) ⇒ if (headers.exists(_.is(h.lowercaseName))) headers else h :: headers
      }
    }

  /**
   * Overrides the media-type of the response returned by its inner route with the given one.
   * If the given media-type is not accepted by the client the request is rejected with an
   * UnacceptedResponseContentTypeRejection.
   * Note, that this directive removes a potentially existing 'Accept' header from the request,
   * in order to "disable" content negotiation in a potentially running Marshaller in its inner route.
   * Also note that this directive does *not* change the response entity buffer content in any way,
   * it merely overrides the media-type component of the entities Content-Type.
   */
  //  def respondWithMediaType(mediaType: MediaType): Directive0 = {
  //    val rejection = UnacceptedResponseContentTypeRejection(ContentType(mediaType) :: Nil)
  //    extract(_.request.isMediaTypeAccepted(mediaType)).flatMap[HNil] { if (_) pass else reject(rejection) } &
  //      mapRequestContext(_.withContentNegotiationDisabled) &
  //      mapRestResponseEntity(_.flatMap { case HttpEntity.NonEmpty(ct, buf) ⇒ HttpEntity(ct.withMediaType(mediaType), buf) }) &
  //      MiscDirectives.cancelRejection(rejection)
  //  }
}

object RespondWithDirectives extends RespondWithDirectives