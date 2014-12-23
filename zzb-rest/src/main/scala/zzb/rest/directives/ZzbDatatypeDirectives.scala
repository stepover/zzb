package zzb.rest.directives

import zzb.datatype._
import zzb.rest._
import zzb.rest.unmarshalling._

/**
 * Created by Simon on 2014/6/24
 */
trait ZzbDatatypeDirectives {

  def unpack(dt: DataType[Any], failHandler: () ⇒ Unit = null) = new Deserializer[RestRequest, ValuePack[Any]] {
    def apply(req: RestRequest): unmarshalling.Deserialized[ValuePack[Any]] = req.entity match {
      case RestEntity.NonEmpty(v) ⇒
        val p = dt.AnyToPack(v)
        p match {
          case Some(pk) ⇒ Right(pk)
          case None ⇒
            if (failHandler != null) failHandler()
            Left(MalformedContent("The request content was malformed"))
        }
      case _ ⇒
        if (failHandler != null) failHandler()
        Left(ContentExpected)
    }
  }

}
