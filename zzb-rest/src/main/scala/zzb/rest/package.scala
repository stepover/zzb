package zzb

import spray.http.{ HttpHeaders, HttpHeader, HttpMethods, HttpMethod }
import shapeless._
import spray.routing.PathMatcher

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-2-27
 * Time: 下午2:05
 * Copyright baoxian.com 2012~2020
 */
package object rest {

  type RestMethod = HttpMethod
  val RestMethods = HttpMethods
  type RestHeader = HttpHeader
  val RestHeader = HttpHeader
  val RestHeaders = HttpHeaders
  type StatusCode = spray.http.StatusCode
  val StatusCodes = spray.http.StatusCodes
  type RestData = Any

  type Route = RestReqContext ⇒ Unit
  type RouteGenerator[T] = T ⇒ Route
  type Directive0 = Directive[HNil]
  type Directive1[T] = Directive[T :: HNil]
  type PathMatcher0 = PathMatcher[HNil]
  type PathMatcher1[T] = PathMatcher[T :: HNil]

  val PostmanName = "RestRequestPostman"
}
