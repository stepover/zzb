package zzb.rest

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-2-25
 * Time: 下午2:38
 * Copyright baoxian.com 2012~2020
 */
sealed trait RestEntity {
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def data: RestData
  def flatMap(f: RestEntity.NonEmpty ⇒ RestEntity): RestEntity
  def orElse(other: RestEntity): RestEntity
  def asString: String
  //def asString(defaultCharset: HttpCharset): String
  def toOption: Option[RestEntity.NonEmpty]
}

object RestEntity {
  implicit def apply(string: String): RestEntity = NonEmpty(string)

  def apply(data: RestData): RestEntity =
    if (data != null) new NonEmpty(data) else Empty

  implicit def flatten(optionalEntity: Option[RestEntity]): RestEntity =
    optionalEntity match {
      case Some(body) ⇒ body
      case None       ⇒ Empty
    }

  /**
   * Models an empty entity.
   */
  case object Empty extends RestEntity {
    def isEmpty = true
    def data = null //HttpData.Empty
    def flatMap(f: RestEntity.NonEmpty ⇒ RestEntity): RestEntity = this
    def orElse(other: RestEntity): RestEntity = other
    def asString = ""
    //def asString(defaultCharset: HttpCharset) = ""
    def toOption = None
  }

  /**
   * Models a non-empty entity. The buffer array is guaranteed to have a size greater than zero.
   * CAUTION: Even though the byte array is directly exposed for performance reasons all instances of this class are
   * assumed to be immutable! spray never modifies the buffer contents after an HttpEntity.NonEmpty instance has been created.
   * If you modify the buffer contents by writing to the array things WILL BREAK!
   */
  case class NonEmpty private[RestEntity] (data: RestData) extends RestEntity {
    def isEmpty = false
    def flatMap(f: RestEntity.NonEmpty ⇒ RestEntity): RestEntity = f(this)
    def orElse(other: RestEntity): RestEntity = this
    def asString = data.toString //asString(contentType.charset)
    //def asString(defaultCharset: HttpCharset) = data.asString(contentType.definedCharset getOrElse defaultCharset)
    def toOption = Some(this)
    //override def toString =
    //  "RestEntity(" + contentType + ',' + (if (data.length > 500) asString.take(500) + "..." else asString) + ')'
  }
}