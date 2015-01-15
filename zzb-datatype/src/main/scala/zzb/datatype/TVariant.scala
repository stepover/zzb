package zzb.datatype

import org.joda.time.DateTime
import zzb.datatype.meta.TypeInfo
import scala.collection.immutable.{WrappedString, StringLike}
import scala.collection.mutable
import scala.reflect._
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by Simon on 2014/6/25
 */
object TVariant extends DataType[Variant] {
  override val vtm: ClassTag[_] = classTag[Variant]

  override def AnyToPack(v: Any): Option[ValuePack[Variant]] = Some(Pack(Variant(v.toString)))

  override def typeInfo = new TypeInfo("Variant","Variant")

  override val t_memo_ : String = "Variant"

  implicit val valueFormat = StringJsonFormat

  def fromJsValue(x: JsValue):Pack  = Pack(Variant(valueFormat.read(x)))

  def apply(v:String):Pack = Pack(Variant(v))

  def apply(v:Byte) :Pack = apply(v.toString)

  def apply(v:Short) :Pack = apply(v.toString)

  def apply(v:Int) :Pack = apply(v.toString)

  def apply(v:Long) :Pack = apply(v.toString)

  def apply(v:Float) :Pack = apply(v.toString)

  def apply(v:Double) :Pack = apply(v.toString)

  def apply(v:Boolean) :Pack = apply(v.toString)

  def apply(v:BigDecimal) :Pack = apply(v.toString())

  def apply(v:DateTime) :Pack = apply(TDateTime.date2String(v))

  implicit def toBoolean(value:Pack): Boolean = value.toBoolean
  implicit def toByte(value:Pack): Byte       = value.toByte
  implicit def toShort(value:Pack): Short     = value.toShort
  implicit def toInt(value:Pack): Int         = value.toInt
  implicit def toLong(value:Pack): Long       = value.toInt
  implicit def toFloat(value:Pack): Float     = value.toLong
  implicit def toDouble(value:Pack): Double   = value.toDouble
  implicit def toDateTime(value:Pack) :DateTime    = value.toDateTime
  implicit def toBigDecimal(value:Pack):BigDecimal = value.toBigDecimal


  case class Pack(value: Variant) extends {
    val dataType = TVariant.this
  } with ValuePack[Variant] {

    override def toJsValue: JsValue =  valueFormat.write(value.inner)

    def validate: List[String] = Nil

    override def equals(that: Any) =
      that.toString == value.inner

    def isNumber = value.isNumber
    def isBoolean = value.isBoolean
    def isDateTime = value.isDateTime

    def toBoolean: Boolean = value.toBoolean
    def toByte: Byte       = value.toByte
    def toShort: Short     = value.toShort
    def toInt: Int         = value.toInt
    def toLong: Long       = value.toInt
    def toFloat: Float     = value.toLong
    def toDouble: Double   = value.toDouble
    def toDateTime :DateTime    = value.toDateTime
    def toBigDecimal:BigDecimal = value.toBigDecimal

    override def toString = value.inner

  }

  object Pack extends DefaultJsonProtocol{
    implicit val packJsonFormat =new RootJsonFormat[Pack]{
      override def read(json: JsValue): Pack = fromJsValue(json)

      override def write(obj: Pack): JsValue = obj.toJsValue
    }

    implicit def fromAny(v :Any) = Pack(Variant(v.toString))
  }
}

case class Variant(inner: String) extends StringLike[String] {

  def isNumber = try {
    BigDecimal(inner)
    true
  } catch {
    case e: Throwable => false
  }

  def isBoolean = inner.toLowerCase match {
    case "true" => true
    case "false" => true
    case _ => false
  }

  def isDateTime = TDateTime.string2DateTime(inner).isDefined

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = mutable.StringBuilder.newBuilder

  def seq = new WrappedString(repr)

  def toDateTime = TDateTime.string2DateTime(inner) match {
    case Some(date) => date
    case None => throw new DateTimeFormatException(s"'$inner' is not valid datetime format")
  }

  def toBigDecimal = BigDecimal(inner)

  override def toString() = inner
}

case class DateTimeFormatException(msg:String) extends IllegalArgumentException