package zzb.rest

package object unmarshalling {

  type Deserialized[T] = Either[DeserializationError, T]
  type FromStringDeserializer[T] = Deserializer[String, T]
  type FromStringOptionDeserializer[T] = Deserializer[Option[String], T]
  type Unmarshaller[T] = Deserializer[RestEntity, T]
  type FromEntityOptionUnmarshaller[T] = Deserializer[Option[RestEntity], T]
  //type FromBodyPartOptionUnmarshaller[T] = Deserializer[Option[BodyPart], T]
  type FromMessageUnmarshaller[T] = Deserializer[RestMessage, T] // source-quote-FromMessageUnmarshaller
  type FromRequestUnmarshaller[T] = Deserializer[RestRequest, T] // source-quote-FromRequestUnmarshaller
  type FromResponseUnmarshaller[T] = Deserializer[RestResponse, T] // source-quote-FromResponseUnmarshaller

  //implicit def formFieldExtractor(form: HttpForm) = FormFieldExtractor(form)
  //implicit def pimpBodyPart(bodyPart: BodyPart) = new PimpedRestEntity(bodyPart.entity)

  implicit class PimpedRestEntity(entity: RestEntity) {
    def as[T](implicit unmarshaller: Unmarshaller[T]): Deserialized[T] = unmarshaller(entity)
  }

  implicit class PimpedRestMessage(msg: RestMessage) {
    def as[T](implicit unmarshaller: FromMessageUnmarshaller[T]): Deserialized[T] = unmarshaller(msg)
  }

  implicit class PimpedRestRequest(request: RestRequest) {
    def as[T](implicit unmarshaller: FromRequestUnmarshaller[T]): Deserialized[T] = unmarshaller(request)
  }

  implicit class PimpedRestResponse(response: RestResponse) {
    def as[T](implicit unmarshaller: FromResponseUnmarshaller[T]): Deserialized[T] = unmarshaller(response)
  }

}
