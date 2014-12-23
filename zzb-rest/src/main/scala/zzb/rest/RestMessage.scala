package zzb.rest

import spray.http.HttpHeaders.Host
import spray.http.Uri
import zzb.datatype.TMono
import scala.annotation.tailrec
import scala.reflect.{ classTag, ClassTag }
import zzb.rest._
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-2-25
 * Time: 下午2:36
 * Copyright baoxian.com 2012~2020
 */
trait RestMessage {
  type Self <: RestMessage

  def message: Self
  def isRequest: Boolean
  def isResponse: Boolean

  def headers: List[RestHeader]
  def entity: RestEntity

  def withHeaders(headers: RestHeader*): Self = withHeaders(headers.toList)
  def withDefaultHeaders(defaultHeaders: List[RestHeader]) = {
    @tailrec def patch(remaining: List[RestHeader], result: List[RestHeader] = headers): List[RestHeader] =
      remaining match {
        case h :: rest if result.exists(_.is(h.lowercaseName)) ⇒ patch(rest, result)
        case h :: rest ⇒ patch(rest, h :: result)
        case Nil ⇒ result
      }
    withHeaders(patch(defaultHeaders))
  }
  def withHeaders(headers: List[RestHeader]): Self
  def withEntity(entity: RestEntity): Self
  def withHeadersAndEntity(headers: List[RestHeader], entity: RestEntity): Self

  //  /** Returns the start part for this message */
  //  def chunkedMessageStart: HttpMessageStart

  def mapHeaders(f: List[RestHeader] ⇒ List[RestHeader]): Self = withHeaders(f(headers))
  def mapEntity(f: RestEntity ⇒ RestEntity): Self = withEntity(f(entity))

  //  /**
  //   * The content encoding as specified by the Content-Encoding header. If no Content-Encoding header is present the
  //   * default value 'identity' is returned.
  //   */
  //  def encoding = header[`Content-Encoding`] match {
  //    case Some(x) ⇒ x.encoding
  //    case None    ⇒ HttpEncodings.identity
  //  }

  def header[T <: RestHeader: ClassTag]: Option[T] = {
    val erasure = classTag[T].runtimeClass
    @tailrec def next(headers: List[RestHeader]): Option[T] =
      if (headers.isEmpty) None
      else if (erasure.isInstance(headers.head)) Some(headers.head.asInstanceOf[T]) else next(headers.tail)
    next(headers)
  }

  //def connectionCloseExpected: Boolean = HttpMessage.connectionCloseExpected(protocol, header[Connection])

  //  /** Returns the message as if it was sent in chunks */
  //  def asPartStream(maxChunkSize: Long = Long.MaxValue): Stream[HttpMessagePart] =
  //    entity match {
  //      case HttpEntity.Empty ⇒ Stream(chunkedMessageStart, ChunkedMessageEnd)
  //      case HttpEntity.NonEmpty(ct, data) ⇒
  //        val start = withHeadersAndEntity(`Content-Type`(ct) :: headers, HttpEntity.Empty).chunkedMessageStart
  //        val chunks: Stream[HttpMessagePart] = data.toChunkStream(maxChunkSize).map(MessageChunk(_))
  //        start #:: chunks append Stream(ChunkedMessageEnd)
  //    }

}

case class RestRequest(method: RestMethod = RestMethods.GET,
                       uri: Uri,
                       headers: List[RestHeader] = Nil,
                       entity: RestEntity = RestEntity.Empty) extends RestMessage {

  require(!uri.isEmpty, "An RestRequest must not have an empty Uri")

  type Self = RestRequest
  def message = this
  def isRequest = true
  def isResponse = false

  def withEffectiveUri(securedConnection: Boolean, defaultHostHeader: Host = Host.empty): RestRequest = {
    val hostHeader = header[Host]
    if (uri.isRelative) {
      def fail(detail: String) =
        sys.error("Cannot establish effective request URI of " + this + ", request has a relative URI and " + detail)
      val Host(host, port) = hostHeader match {
        case None                 ⇒ if (defaultHostHeader.isEmpty) fail("is missing a `Host` header") else defaultHostHeader
        case Some(x) if x.isEmpty ⇒ if (defaultHostHeader.isEmpty) fail("an empty `Host` header") else defaultHostHeader
        case Some(x)              ⇒ x
      }
      copy(uri = uri.toEffectiveHttpRequestUri(Uri.Host(host), port, securedConnection))
    } else // http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-22#section-5.4
    if (hostHeader.isEmpty || uri.authority.isEmpty && hostHeader.get.isEmpty ||
      hostHeader.get.host.equalsIgnoreCase(uri.authority.host.address) && hostHeader.get.port == uri.authority.port) this
    else sys.error("'Host' header value doesn't match request target authority")
  }

  def withHeaders(headers: List[RestHeader]) = if (headers eq this.headers) this else copy(headers = headers)

  def withEntity(entity: RestEntity) = if (entity eq this.entity) this else copy(entity = entity)
  def withHeadersAndEntity(headers: List[RestHeader], entity: RestEntity) =
    if ((headers eq this.headers) && (entity eq this.entity)) this else copy(headers = headers, entity = entity)

}

//@SerialVersionUID(1L)
case class RestResponse(status: StatusCode = StatusCodes.OK,
                        entity: RestEntity = RestEntity.Empty,
                        headers: List[RestHeader] = Nil) extends RestMessage {
  type Self = RestResponse

  def message = this
  def isRequest = false
  def isResponse = true

  def withHeaders(headers: List[RestHeader]) = if (headers eq this.headers) this else copy(headers = headers)
  def withEntity(entity: RestEntity) = if (entity eq this.entity) this else copy(entity = entity)
  def withHeadersAndEntity(headers: List[RestHeader], entity: RestEntity) =
    if ((headers eq this.headers) && (entity eq this.entity)) this else copy(headers = headers, entity = entity)
}

object RestResponse {
  def to[T](r: RestResponse)(implicit trans: PartialFunction[Any, T] = null,
                             notOk: RestResponse ⇒ Throwable = r ⇒ new RequestFailed(r, if (r.entity.nonEmpty) r.entity.asString else ""),
                             m: ClassTag[T]): T = {
    def boxedType(rtClass: Class[_]) = rtClass match {
      case java.lang.Byte.TYPE      ⇒ Class.forName("java.lang.Byte")
      case java.lang.Short.TYPE     ⇒ Class.forName("java.lang.Short")
      case java.lang.Character.TYPE ⇒ Class.forName("java.lang.Character")
      case java.lang.Integer.TYPE   ⇒ Class.forName("java.lang.Integer")
      case java.lang.Long.TYPE      ⇒ Class.forName("java.lang.Long")
      case java.lang.Float.TYPE     ⇒ Class.forName("java.lang.Float")
      case java.lang.Double.TYPE    ⇒ Class.forName("java.lang.Double")
      case java.lang.Boolean.TYPE   ⇒ Class.forName("java.lang.Boolean")
      case rtc                      ⇒ rtc
    }
    if (r.status != StatusCodes.OK) throw notOk(r)
    if (r.entity.isEmpty) throw ResponseNonEntity(r)
    val data = r.entity.data
    if (trans == null || !trans.isDefinedAt(data)) {
      data match {
        case p: TMono[_]#Pack if p.dataType.vtm == m ⇒
          p.value.asInstanceOf[T]
        case v if boxedType(m.runtimeClass).isInstance(v) ⇒
          r.entity.data.asInstanceOf[T]
        case _ ⇒
          throw new ResponseWrongType(r, s"${data.getClass.toString} can't be cast to ${m.runtimeClass.toString}")
      }
    } else {
      trans(data)
    }
  }
  def toOrElse[T](r: RestResponse, default: ⇒ T)(implicit m: Manifest[T], trans: PartialFunction[Any, T] = null): T = {
    try {
      to[T](r)
    } catch {
      case e: Throwable ⇒
        default
    }
  }
  def toOrElseForStatus[T](r: RestResponse, default: ⇒ T,
                           forStatus: StatusCode ⇒ Boolean)(implicit m: Manifest[T], trans: PartialFunction[Any, T] = null): T = {
    try {
      to[T](r)
    } catch {
      case e: RequestFailed if forStatus(e.res.status) ⇒ default
      case e: Throwable                                ⇒ throw e
    }

  }
  def toOrElseInStatus[T](r: RestResponse, default: ⇒ T,
                          inStatus: StatusCode*)(implicit m: Manifest[T], trans: PartialFunction[Any, T] = null): T = {

    toOrElseForStatus(r, default, inStatus.contains)
  }

  def toOrElseNotStatus[T](r: RestResponse, default: ⇒ T,
                           inStatus: StatusCode*)(implicit m: Manifest[T], trans: PartialFunction[Any, T] = null): T = {
    toOrElseForStatus(r, default, !inStatus.contains(_))
  }
}

case class RequestFailed(res: RestResponse, msg: String) extends Exception(msg)
case class ResponseNonEntity(res: RestResponse) extends Exception
case class ResponseWrongType(res: RestResponse, msg: String) extends Exception(msg)
