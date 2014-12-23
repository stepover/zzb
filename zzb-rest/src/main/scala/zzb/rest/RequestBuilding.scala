package zzb.rest

import spray.http.Uri
import scala.reflect.ClassTag
import akka.event.{ Logging, LoggingAdapter }
import marshalling._
import RestMethods._
import RestHeaders._
import spray.http.parser.HttpParser

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-1
 * Time: 上午11:20
 * Copyright baoxian.com 2012~2020
 */
trait RequestBuilding extends TransformerPipelineSupport {
  type RequestTransformer = RestRequest ⇒ RestRequest

  class RequestBuilder(val method: RestMethod) {
    def apply(): RestRequest = apply("/")
    def apply(uri: String): RestRequest = apply[String](uri, None)
    def apply[T: Marshaller](uri: String, content: T): RestRequest = apply(uri, Some(content))
    def apply[T: Marshaller](uri: String, content: Option[T]): RestRequest = apply(Uri(uri), content)
    def apply(uri: Uri): RestRequest = apply[String](uri, None)
    def apply[T: Marshaller](uri: Uri, content: T): RestRequest = apply(uri, Some(content))
    def apply[T: Marshaller](uri: Uri, content: Option[T]): RestRequest = {
      val ctx = new CollectingMarshallingContext
      content match {
        case None ⇒ RestRequest(method, uri)
        case Some(value) ⇒ marshalToEntityAndHeaders(value, ctx) match {
          case Right((entity, headers)) ⇒ RestRequest(method, uri, headers.toList, entity)
          case Left(error)              ⇒ throw error
        }
      }
    }
  }

  val Get = new RequestBuilder(GET)
  val Post = new RequestBuilder(POST)
  val Put = new RequestBuilder(PUT)
  val Patch = new RequestBuilder(PATCH)
  val Delete = new RequestBuilder(DELETE)
  val Options = new RequestBuilder(OPTIONS)
  val Head = new RequestBuilder(HEAD)

  //def encode(encoder: Encoder): RequestTransformer = encoder.encode(_)

  def addHeader(header: RestHeader): RequestTransformer = _.mapHeaders(header :: _)

  def addHeader(headerName: String, headerValue: String): RequestTransformer = {
    val rawHeader = RawHeader(headerName, headerValue)
    addHeader(HttpParser.parseHeader(rawHeader).left.flatMap(_ ⇒ Right(rawHeader)).right.get)
  }

  def addHeaders(first: RestHeader, more: RestHeader*): RequestTransformer = addHeaders(first :: more.toList)

  def addHeaders(headers: List[RestHeader]): RequestTransformer = _.mapHeaders(headers ::: _)

  def mapHeaders(f: List[RestHeader] ⇒ List[RestHeader]): RequestTransformer = _.mapHeaders(f)

  def removeHeader(headerName: String): RequestTransformer = {
    val selected = (_: RestHeader).name equalsIgnoreCase headerName
    _ mapHeaders (_ filterNot selected)
  }

  def removeHeader[T <: RestHeader: ClassTag]: RequestTransformer = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    val selected = (header: RestHeader) ⇒ clazz.isInstance(header)
    _ mapHeaders (_ filterNot selected)
  }

  def removeHeaders(names: String*): RequestTransformer = {
    val selected = (header: RestHeader) ⇒ names exists (_ equalsIgnoreCase header.name)
    _ mapHeaders (_ filterNot selected)
  }

  //def addCredentials(credentials: HttpCredentials) = addHeader(RestHeaders.Authorization(credentials))

  def logRequest(log: LoggingAdapter, level: Logging.LogLevel = Logging.DebugLevel) = logValue[RestRequest](log, level)

  def logRequest(logFun: RestRequest ⇒ Unit) = logValue[RestRequest](logFun)

  implicit def header2AddHeader(header: RestHeader): RequestTransformer = addHeader(header)
}

object RequestBuilding extends RequestBuilding
