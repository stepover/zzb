package zzb.rest
package unmarshalling

import java.nio.ByteBuffer
import java.io.{ InputStreamReader, ByteArrayInputStream }
import scala.xml.{ XML, NodeSeq }

trait BasicUnmarshallers {

  //  implicit val ByteArrayUnmarshaller = new Unmarshaller[Array[Byte]] {
  //    def apply(entity: RestEntity) = Right(entity.data.toByteArray)
  //  }
  //
  //  implicit val CharArrayUnmarshaller = new Unmarshaller[Array[Char]] {
  //    def apply(entity: RestEntity) = Right { // we can convert anything to a char array
  //      entity match {
  //        case RestEntity.NonEmpty(contentType, data) ⇒
  //          val charBuffer = contentType.charset.nioCharset.decode(ByteBuffer.wrap(data.toByteArray))
  //          val array = new Array[Char](charBuffer.length())
  //          charBuffer.get(array)
  //          array
  //        case RestEntity.Empty ⇒ Array.empty[Char]
  //      }
  //    }
  //  }

  implicit val StringUnmarshaller = new Unmarshaller[String] {
    def apply(entity: RestEntity) = Right(entity.asString)
  }

  //  //# nodeseq-unmarshaller
  //  implicit val NodeSeqUnmarshaller =
  //    Unmarshaller[NodeSeq](`text/xml`, `application/xml`, `text/html`, `application/xhtml+xml`) {
  //      case RestEntity.NonEmpty(contentType, data) ⇒
  //        XML.load(new InputStreamReader(new ByteArrayInputStream(data.toByteArray), contentType.charset.nioCharset))
  //      case RestEntity.Empty ⇒ NodeSeq.Empty
  //    }
  //  //#
}

object BasicUnmarshallers extends BasicUnmarshallers
