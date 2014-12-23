package zzb.rest
package marshalling

import akka.util.ByteString

//import spray.http._
//import MediaTypes._

trait BasicMarshallers {

  implicit val ByteArrayMarshaller = byteArrayMarshaller()

  def byteArrayMarshaller(): Marshaller[Array[Byte]] =
    Marshaller.of[Array[Byte]]() {
      (value, ctx) ⇒
        // we marshal to the ContentType given as argument to the method, not the one established by content-negotiation,
        // since the former is the one belonging to the byte array
        ctx.marshalTo(RestEntity(value))
    }

  implicit val ByteStringMarshaller = byteStringMarshaller()

  def byteStringMarshaller(): Marshaller[ByteString] =
    Marshaller.of[ByteString]() {
      (value, ctx) ⇒
        // we marshal to the ContentType given as argument to the method, not the one established by content-negotiation,
        // since the former is the one belonging to the ByteString
        ctx.marshalTo(RestEntity(value))
    }

  implicit val RestDataMarshaller = restDataMarshaller()

  def restDataMarshaller(): Marshaller[RestData] =
    Marshaller.of[RestData]() {
      (value, ctx) ⇒
        // we marshal to the ContentType given as argument to the method, not the one established by content-negotiation,
        // since the former is the one belonging to the RestData
        ctx.marshalTo(RestEntity(value))
    }

  implicit val CharArrayMarshaller =
    Marshaller.of[Array[Char]]() {
      (value, ctx) ⇒
        ctx.marshalTo {
          if (value.length > 0) {
            RestEntity(value)
          } else RestEntity.Empty
        }
    }

  //# string-marshaller
  // prefer UTF-8 encoding, but also render with other encodings if the client requests them
  implicit val StringMarshaller = stringMarshaller

  def stringMarshaller: Marshaller[String] =
    Marshaller.of[String]() {
      (value, ctx) ⇒
        ctx.marshalTo(RestEntity(value))
    }

  //#

  //  //# nodeseq-marshaller
  //  implicit val NodeSeqMarshaller =
  //    Marshaller.delegate[NodeSeq, String](`text/xml`, `application/xml`,
  //      `text/html`, `application/xhtml+xml`)(_.toString)
  //  //#

  //  implicit val FormDataMarshaller =
  //    Marshaller.delegate[FormData, String](`application/x-www-form-urlencoded`) { (formData, contentType) ⇒
  //      val charset = contentType.charset.nioCharset
  //      Uri.Query(formData.fields: _*).render(new StringRendering, charset).get
  //    }

  implicit val ThrowableMarshaller = Marshaller[Throwable] {
    (value, ctx) ⇒ ctx.handleError(value)
  }

  implicit val RestEntityMarshaller = Marshaller[RestEntity] {
    (value, ctx) ⇒
      value match {
        case RestEntity.Empty ⇒ ctx.marshalTo(RestEntity.Empty)
        case _                ⇒ ctx.marshalTo(value)
      }
  }
}

object BasicMarshallers extends BasicMarshallers