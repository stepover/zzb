package zzb.rest.http2akka

import akka.actor.{ ActorRef, ActorSystem, Props }
import akka.util.Timeout
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.{ Marshaller, ToResponseMarshaller, ToResponseMarshallingContext }
import spray.httpx.unmarshalling
import spray.httpx.unmarshalling.{ ContentExpected, MalformedContent, Deserializer }
import spray.json._
import spray.routing.{ RequestContext, Route, UnacceptedResponseContentTypeRejection }
import zzb.datatype.{ ValuePack, DataType }
import zzb.rest.RestEntity.{ Empty, NonEmpty }
import zzb.rest.{ RestEntity, RestRequest, Requester, RestResponse, EntityResponder }
import zzb.util.MdcLoggingContext
import scala.reflect.ClassTag

/**
 * Created by Simon on 2014/3/20
 */
trait Http2AkkaDirectives {

  def valueTranslator: PartialFunction[Any, ValuePack[Any]] = Http2AkkaDirectives.basicType2ValuePack

  def domainCompleteAs(dt: DataType[Any])(implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext): Route = {
    implicit val format = ValuePackFormat(dt)

    akkaCompleteAs[ValuePack[Any]](timeout, system, mdcLog, sprayJsonMarshaller[ValuePack[Any]](format), manifest[ValuePack[Any]], valueTranslator)
  }

  def domainWithEntityCompleteAs(dt: DataType[Any], entity: AnyRef)(implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext): Route = {
    implicit val format = ValuePackFormat(dt)
    akkaWithEntityCompleteAs[ValuePack[Any]](entity)(timeout, system, mdcLog, sprayJsonMarshaller[ValuePack[Any]](format), manifest[ValuePack[Any]], valueTranslator)
  }

  def unpack(dt: DataType[Any]) = new Deserializer[HttpRequest, ValuePack[Any]] {
    def apply(req: HttpRequest): unmarshalling.Deserialized[ValuePack[Any]] = req.entity match {
      case x: HttpEntity.NonEmpty ⇒
        try {
          val json = JsonParser(x.asString(defaultCharset = HttpCharsets.`UTF-8`))
          Right(dt.fromJsValue(json))
        } catch {
          case e: Throwable ⇒
            Left(MalformedContent(e.getMessage, e))
        }

      case _ ⇒ Left(ContentExpected)
    }
  }

  implicit def sprayJsonListMarshaller[T](implicit writer: RootJsonWriter[T], printer: JsonPrinter = PrettyPrinter) =
    Marshaller.delegate[List[T], String](ContentTypes.`application/json`) { value ⇒
      val jsArray = new JsArray(value.map(writer.write))
      printer(jsArray)
    }

  private def innerAkkaCompleteAs[Out](entity: RestEntity)(implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[Out], m: ClassTag[Out], trans: PartialFunction[Any, Out] = null): Route =
    httpCtx ⇒ {
      val httpReq = httpCtx.request
      val restReq = RestRequest(httpReq.method, httpReq.uri.toRelative, httpReq.headers, entity)

      val responder = system.actorOf(Props(new HttpResponder[Out](httpCtx)))

      Requester.exeRequest(restReq, responder)
    }

  def akkaWithEntityCompleteAs[Out](entity: AnyRef)(implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[Out], m: Manifest[Out], trans: PartialFunction[Any, Out] = null): Route =
    innerAkkaCompleteAs[Out](RestEntity(entity))

  def akkaCompleteAs[Out](implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[Out], m: Manifest[Out], trans: PartialFunction[Any, Out] = null): Route =
    innerAkkaCompleteAs[Out](RestEntity.Empty)

  def akkaCompleteAsList[Out](implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[List[Out]], m: Manifest[Out], trans: PartialFunction[Any, Out] = null): Route =
    innerAkkaCompleteAs[List[Out]](RestEntity.Empty)

  def akkaWithEntityCompleteAsList[Out](entity: AnyRef)(implicit timeout: Timeout, system: ActorSystem, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[List[Out]], m: Manifest[Out], trans: PartialFunction[Any, Out] = null): Route =
    innerAkkaCompleteAs[List[Out]](RestEntity(entity))
}

private[http2akka] class HttpResponder[T](httpCtx: RequestContext)(implicit to: Timeout, mdcLog: MdcLoggingContext, marshaller: ToResponseMarshaller[T], m: ClassTag[T], trans: PartialFunction[Any, T] = null) extends EntityResponder[T] {

  override def timeout = Some(to)

  override def requestId = httpCtx.request.method.value + " " + httpCtx.request.uri.toString()

  override def requestHeaders = httpCtx.request.headers

  private def marshallerAndResponseToHttp(v: T, resp: RestResponse) = {
    val httpReq = httpCtx.request
    val ctx = new ToResponseMarshallingContext {
      def tryAccept(contentTypes: Seq[ContentType]) = httpReq.acceptableContentType(contentTypes)

      def rejectMarshalling(onlyTo: Seq[ContentType]): Unit = httpCtx.reject(UnacceptedResponseContentTypeRejection(onlyTo))

      def marshalTo(response: HttpResponse): Unit =
        httpCtx.responder ! response.withHeaders(resp.headers).copy(status = resp.status)

      def handleError(error: Throwable): Unit = httpCtx.failWith(error)

      def startChunkedMessage(response: HttpResponse, sentAck: Option[Any])(implicit sender: ActorRef) = {
        val chunkStart = ChunkedResponseStart(response)
        val wrapper = if (sentAck.isEmpty) chunkStart else Confirmed(chunkStart, sentAck.get)
        httpCtx.responder.tell(wrapper, sender)
        httpCtx.responder
      }
    }
    marshaller(v.asInstanceOf[T], ctx)
  }

  override def onTimeout(): Unit = httpCtx.complete(StatusCodes.InternalServerError, s"request to '$requestId' timeout in ${timeout.get.duration.toString()} ")

  override def onFailed(e: Throwable) = httpCtx.failWith(e)

  override def onSuccess(value: T): Unit = {
    val resp = response.get
    marshallerAndResponseToHttp(value, resp)
  }

  override def onSuccess() = {
    val resp = response.get
    httpCtx.complete(HttpResponse(resp.status, HttpEntity.Empty, resp.headers))
    super.onSuccess()
  }

  override def onWrongType(v: Any) = {
    val resp = response.get
    httpCtx.complete(HttpResponse(resp.status, HttpEntity(v.toString), resp.headers))
    super.onWrongType(v)
  }

  private def restResponseProcess(resp: RestResponse) = resp.entity match {
    case Empty ⇒ httpCtx.complete(HttpResponse(resp.status, HttpEntity.Empty, resp.headers))
    case NonEmpty(v) if m.runtimeClass.isInstance(v) ⇒
      marshallerAndResponseToHttp(v.asInstanceOf[T], resp)
    case NonEmpty(v) ⇒ httpCtx.complete(HttpResponse(resp.status, HttpEntity(v.toString), resp.headers))
  }

  status(201 to 510) { resp ⇒
    restResponseProcess(resp)
  }
}

object Http2AkkaDirectives extends Http2AkkaDirectives {
  import zzb.datatype._
  //将基本类型转化成 valuePack
  implicit val basicType2ValuePack: PartialFunction[Any, ValuePack[Any]] = {
    case v: Byte    ⇒ TByte(v)
    case v: Short   ⇒ TShort(v)
    case v: Int     ⇒ TInt(v)
    case v: Long    ⇒ TLong(v)
    case v: Float   ⇒ TFloat(v)
    case v: Double  ⇒ TDouble(v)
    case v: Boolean ⇒ TBoolean(v)
    case v: String  ⇒ TString(v)
  }
}

case class ValuePackFormat(dt: DataType[Any]) extends RootJsonFormat[ValuePack[Any]] {
  override def read(json: JsValue): ValuePack[Any] = dt.fromJsValue(json)

  override def write(pack: ValuePack[Any]): JsValue = pack.toJsValue
}

