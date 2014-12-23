package zzb.rest

import zzb.rest.marshalling._
import spray.http.Uri
import spray.http.HttpMethods._
import zzb.util.MdcLoggingContext
import scala.concurrent.Future
import akka.util.Timeout
import akka.actor.{ ActorRef, ActorSystem }

/**
 * Created by Simon on 2014/5/20
 */
trait Caller extends Requester {

  class CallBuilder(val method: RestMethod) {
    def apply()(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] = apply("/")

    def apply(uri: String)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] = apply[String](uri, None)

    def apply[T](uri: String, content: T)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] = apply(uri, Some(content))

    def apply[T](uri: String, content: Option[T])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] = apply(Uri(uri), content, Nil)

    def apply(uri: Uri)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] = apply[String](uri, None, Nil)

    def apply[T](uri: Uri, content: T)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] = apply(uri, Some(content), Nil)

    // with headers

    def apply(headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] = apply("/", headers)

    def apply(uri: String, headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] =
      apply[String](uri, None, headers)

    def apply[T](uri: String, content: T, headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] =
      apply(uri, Some(content), headers)

    def apply[T](uri: String, content: Option[T], headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] =
      apply(Uri(uri), content, headers)

    def apply(uri: Uri, headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Future[RestResponse] =
      apply[String](uri, None, headers)

    def apply[T](uri: Uri, content: T, headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] =
      apply(uri, Some(content), headers)

    // final impl
    def apply[T](uri: Uri, content: Option[T], headers: List[RestHeader])(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Future[RestResponse] = {
      val ctx = new CollectingMarshallingContext
      //val callUrl = if (uri.isAbsolute || rootUri.isEmpty) uri else uri.resolvedAgainst(rootUri)
      val req = content match {
        case None ⇒ RestRequest(method, uri).withHeaders(headers)
        case Some(value) ⇒ marshalToEntityAndHeaders(value, ctx) match {
          case Right((entity, reqHeaders)) ⇒
            RestRequest(method, uri, reqHeaders.toList ++ headers, entity)
          case Left(error) ⇒ throw error
        }
      }
      exeRequest(req)
    }

    def apply(responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = apply("/", responder)

    def apply(uri: String, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = apply[String](uri, None, responder)

    def apply[T](uri: String, content: T, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit = apply(uri, Some(content), responder)

    def apply[T](uri: String, content: Option[T], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit = apply(Uri(uri), content, Nil, responder)

    def apply(uri: Uri, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = apply[String](uri, None, Nil, responder)

    def apply[T](uri: Uri, content: T, responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit = apply(uri, Some(content), Nil, responder)

    // with headers

    def apply(headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit = apply("/", headers, responder)

    def apply(uri: String, headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit =
      apply[String](uri, None, headers, responder)

    def apply[T](uri: String, content: T, headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit =
      apply(uri, Some(content), headers, responder)

    def apply[T](uri: String, content: Option[T], headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit =
      apply(Uri(uri), content, headers, responder)

    def apply(uri: Uri, headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext): Unit =
      apply[String](uri, None, headers, responder)

    def apply[T](uri: Uri, content: T, headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit =
      apply(uri, Some(content), headers, responder)

    def apply[T](uri: Uri, content: Option[T], headers: List[RestHeader], responder: ActorRef)(implicit timeout: Timeout, system: ActorSystem, rs: RestSettings, log: MdcLoggingContext, m: Marshaller[T]): Unit = {
      val ctx = new CollectingMarshallingContext
      //val callUrl = if (uri.isAbsolute || rootUri.isEmpty) uri else uri.resolvedAgainst(rootUri)
      val req = content match {
        case None ⇒ RestRequest(method, uri).withHeaders(headers)
        case Some(value) ⇒ marshalToEntityAndHeaders(value, ctx) match {
          case Right((entity, reqHeaders)) ⇒
            RestRequest(method, uri, reqHeaders.toList ++ headers, entity)
          case Left(error) ⇒ throw error
        }
      }
      exeRequest(req, responder)
    }
  }

  val doGet = new CallBuilder(GET)
  val doPost = new CallBuilder(POST)
  val doPut = new CallBuilder(PUT)
  val doPatch = new CallBuilder(PATCH)
  val doDelete = new CallBuilder(DELETE)
  val doOptions = new CallBuilder(OPTIONS)
  val doHead = new CallBuilder(HEAD)
}

object Caller extends Caller
