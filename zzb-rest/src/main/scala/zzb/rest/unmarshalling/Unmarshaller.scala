package zzb.rest
package unmarshalling

object Unmarshaller {
  // format: OFF
  def apply[T](f: PartialFunction[RestEntity, T]): Unmarshaller[T] = // unmarshaller-apply
    // format: ON
    new SimpleUnmarshaller[T] {
      def unmarshal(entity: RestEntity) =
        if (f.isDefinedAt(entity)) protect(f(entity)) else Left(ContentExpected)
    }

  def delegate[A, B](f: A ⇒ B)(implicit mb: Unmarshaller[A]): Unmarshaller[B] =
    new SimpleUnmarshaller[B] {
      def unmarshal(entity: RestEntity) = mb(entity).right.flatMap(a ⇒ protect(f(a)))
    }

  def forNonEmpty[T](implicit um: Unmarshaller[T]): Unmarshaller[T] =
    new Unmarshaller[T] {
      def apply(entity: RestEntity) = if (entity.isEmpty) Left(ContentExpected) else um(entity)
    }

  def unmarshaller[T](implicit um: Unmarshaller[T]) = um

  def unmarshal[T: Unmarshaller](entity: RestEntity): Deserialized[T] = unmarshaller.apply(entity)
  def unmarshalUnsafe[T: Unmarshaller](entity: RestEntity): T = unmarshaller.apply(entity) match {
    case Right(value) ⇒ value
    case Left(error)  ⇒ sys.error(error.toString)
  }

  //  def oneOf[T](unmarshallers: Unmarshaller[T]*): Unmarshaller[T] =
  //    new Unmarshaller[T] {
  //      def apply(entity: RestEntity): Deserialized[T] = {
  //        def tryNext(unmarshallers: Seq[Unmarshaller[T]]): Deserialized[T] = unmarshallers match {
  //          case head +: tail ⇒ head(entity).left.flatMap(_ ⇒ tryNext(tail))
  //          case Nil ⇒
  //            def tpeString = entity match {
  //              case RestEntity.NonEmpty(tpe, _) ⇒ tpe.value
  //              case RestEntity.Empty            ⇒ "an empty entity"
  //            }
  //            Left(UnsupportedContentType("Can't unmarshal from " + tpeString))
  //        }
  //        tryNext(unmarshallers)
  //      }
  //    }
}
