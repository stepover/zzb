package zzb.rest
package directives

import scala.util.control.NonFatal
import shapeless._
import spray.util._

trait HeaderDirectives {
  import BasicDirectives._
  import RouteDirectives._

  /**
   * Extracts an HTTP header value using the given function. If the function result is undefined for all headers the
   * request is rejected with an empty rejection set. If the given function throws an exception the request is rejected
   * with a [[zzb.rest.MalformedHeaderRejection]].
   */
  def headerValue[T](f: RestHeader ⇒ Option[T]): Directive1[T] = {
    val protectedF: RestHeader ⇒ Option[Either[Rejection, T]] = header ⇒
      try f(header).map(Right.apply)
      catch {
        case NonFatal(e) ⇒ Some(Left(MalformedHeaderRejection(header.name, e.getMessage.nullAsEmpty, Some(e))))
      }
    extract(_.request.headers.mapFind(protectedF)).flatMap {
      case Some(Right(a))        ⇒ provide(a)
      case Some(Left(rejection)) ⇒ reject(rejection)
      case None                  ⇒ reject
    }
  }

  /**
   * Extracts an HTTP header value using the given partial function. If the function is undefined for all headers the
   * request is rejected with an empty rejection set.
   */
  def headerValuePF[T](pf: PartialFunction[RestHeader, T]): Directive1[T] = headerValue(pf.lift)

  /**
   * Extracts the value of the HTTP request header with the given name.
   * If no header with a matching name is found the request is rejected with a [[zzb.rest.MissingHeaderRejection]].
   */
  def headerValueByName(headerName: Symbol): Directive1[String] = headerValueByName(headerName.toString)

  /**
   * Extracts the value of the HTTP request header with the given name.
   * If no header with a matching name is found the request is rejected with a [[zzb.rest.MissingHeaderRejection]].
   */
  def headerValueByName(headerName: String): Directive1[String] =
    headerValue(optionalValue(headerName.toLowerCase)) | reject(MissingHeaderRejection(headerName))

  /**
   * Extracts an optional HTTP header value using the given function.
   * If the given function throws an exception the request is rejected
   * with a [[zzb.rest.MalformedHeaderRejection]].
   */
  def optionalHeaderValue[T](f: RestHeader ⇒ Option[T]): Directive1[Option[T]] =
    headerValue(f).map(Some(_): Option[T]).recoverPF {
      case Nil ⇒ provide(None)
    }

  /**
   * Extracts an optional HTTP header value using the given partial function.
   * If the given function throws an exception the request is rejected
   * with a [[zzb.rest.MalformedHeaderRejection]].
   */
  def optionalHeaderValuePF[T](pf: PartialFunction[RestHeader, T]): Directive1[Option[T]] =
    optionalHeaderValue(pf.lift)

  /**
   * Extracts the value of the optional HTTP request header with the given name.
   */
  def optionalHeaderValueByName(headerName: Symbol): Directive1[Option[String]] =
    optionalHeaderValueByName(headerName.toString)

  /**
   * Extracts the value of the optional HTTP request header with the given name.
   */
  def optionalHeaderValueByName(headerName: String): Directive1[Option[String]] = {
    val f = optionalValue(headerName.toLowerCase)
    extract(_.request.headers.mapFind(f))
  }

  private def optionalValue(lowerCaseName: String): RestHeader ⇒ Option[String] = {
    case RestHeader(`lowerCaseName`, value) ⇒ Some(value)
    case _                                  ⇒ None
  }
}

object HeaderDirectives extends HeaderDirectives