package zzb.rest
package directives

import shapeless._
import zzb.datatype.TStruct
import zzb.rest.RestEntity._
import zzb.rest.marshalling._
import zzb.rest.unmarshalling._

trait MarshallingDirectives {

  import BasicDirectives._
  import MiscDirectives._
  import RouteDirectives._

  /**
   * Unmarshalls the requests entity to the given type passes it to its inner Route.
   * If there is a problem with unmarshalling the request is rejected with the [[zzb.rest.Rejection]]
   * produced by the unmarshaller.
   */
  def entity[T](um: FromRequestUnmarshaller[T]): Directive1[T] =
    extract(_.request.as(um)).flatMap[T :: HNil] {
      case Right(value)                            ⇒ provide(value)
      case Left(ContentExpected)                   ⇒ reject(RequestEntityExpectedRejection)
      //case Left(UnsupportedContentType(supported)) ⇒ reject(UnsupportedRequestContentTypeRejection(supported))
      case Left(MalformedContent(errorMsg, cause)) ⇒ reject(MalformedRequestContentRejection(errorMsg, cause))
    } & cancelAllRejections(ofTypes(RequestEntityExpectedRejection.getClass, classOf[UnsupportedRequestContentTypeRejection]))

  /**
   * 解析 TStruct 文档数据并做校验
   */
  def packEntity[T <: TStruct#Pack](um: FromRequestUnmarshaller[T], trans: T ⇒ T = (v: T) ⇒ v): Directive1[T] =
    extract(_.request.as(um)).flatMap[T :: HNil] {
      case Right(v) ⇒
        val value = trans(v)
        value.validate match {
          case Nil       ⇒ provide(value)
          case errorMsgs ⇒ complete(StatusCodes.BadRequest, errorMsgs.mkString(";"))
        }
      case Left(ContentExpected) ⇒ reject(RequestEntityExpectedRejection)
      case Left(MalformedContent(errorMsg, cause)) ⇒
        reject(MalformedRequestContentRejection(errorMsg, cause))
    } & cancelAllRejections(ofTypes(RequestEntityExpectedRejection.getClass, classOf[UnsupportedRequestContentTypeRejection]))

  def hasEntity: Directive1[Boolean] = extract(_.request.entity).flatMap {
    case Empty ⇒
      provide(false)
    case _ ⇒
      provide(true)
  }

  /**
   * Returns the in-scope FromRequestUnmarshaller for the given type.
   */
  def as[T](implicit um: FromRequestUnmarshaller[T]) = um

  /**
   * Uses the marshaller for the given type to produce a completion function that is passed to its inner route.
   * You can use it do decouple marshaller resolution from request completion.
   */
  //  def produce[T](marshaller: ToResponseMarshaller[T]): Directive[(T ⇒ Unit) :: HNil] =
  //    extract { ctx ⇒ (value: T) ⇒ ctx.complete(value)(marshaller) } & cancelAllRejections(ofType[UnsupportedRequestContentTypeRejection])

  /**
   * Returns the in-scope Marshaller for the given type.
   */
  def instanceOf[T](implicit m: ToResponseMarshaller[T]) = m

  /**
   * Completes the request using the given function. The input to the function is produced with the in-scope
   * entity unmarshaller and the result value of the function is marshalled with the in-scope marshaller.
   */
  def handleWith[A, B](f: A ⇒ B)(implicit um: FromRequestUnmarshaller[A], m: ToResponseMarshaller[B]): Route =
    entity(um) { a ⇒ RouteDirectives.complete(f(a)) }

  implicit def RestEntityUnmarshaller[T](implicit m: Manifest[T]) = new Deserializer[RestRequest, T] {
    def apply(req: RestRequest): unmarshalling.Deserialized[T] = req.entity match {
      case RestEntity.NonEmpty(v) ⇒
        val vt = v.asInstanceOf[T]
        if (m.runtimeClass.isInstance(v))
          Right(vt)
        else Left(MalformedContent("The request content was malformed"))

      case _ ⇒ Left(ContentExpected)

    }
  }
}

object MarshallingDirectives extends MarshallingDirectives