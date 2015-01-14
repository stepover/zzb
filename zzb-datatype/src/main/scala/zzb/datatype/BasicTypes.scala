package zzb.datatype

import java.util.concurrent.atomic.AtomicReference

import com.github.nscala_time.time.Imports
//import spray.json.DefaultJsonProtocol._
import BasicJsonFormats._
import spray.json._
import zzb.datatype.meta.EnumTypeInfo

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午5:08
 * Copyright baoxian.com 2012~2020
 */

trait TString extends TMono[String] {

  val vtm = classTag[String]

  def parse(str: String): Pack = Pack(str)
  implicit val valueFormat = TStringJsonFormat
}

object TString extends TString {
  override val t_memo_ : String = "String"

  def apply(code:String,memo:String) = new TString{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}

trait TInt extends TMono[Int] {
  val vtm = classTag[Int]
  def parse(str: String): Pack = Pack(str.toInt)

  implicit val valueFormat = TIntJsonFormat
}

object TInt extends TInt {
  override val t_memo_ : String = "Int"
  def apply(code:String,memo:String) = new TInt{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}


trait TLong extends TMono[Long] {
  val vtm = classTag[Long]
  def parse(str: String): Pack = Pack(str.toLong)
  implicit val valueFormat = TLongJsonFormat
}

object TLong extends TLong {
  override val t_memo_ : String = "Long"
  def apply(code:String,memo:String) = new TLong{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}


trait TShort extends TMono[Short] {
  val vtm = classTag[Short]
  def parse(str: String): Pack = Pack(str.toShort)
  implicit val valueFormat = TShortJsonFormat
}

object TShort extends TShort {
  override val t_memo_ : String = "Short"
  def apply(code:String,memo:String) = new TShort{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}

trait TByte extends TMono[Byte] {
  val vtm = classTag[Byte]
  def parse(str: String): Pack = Pack(str.toByte)
  implicit val valueFormat = TByteJsonFormat
}

object TByte extends TByte {
  override val t_memo_ : String = "Byte"
  def apply(code:String,memo:String) = new TByte{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}

trait TDouble extends TMono[Double] {
  val vtm = classTag[Double]
  def parse(str: String): Pack = Pack(str.toDouble)
  implicit val valueFormat = TDoubleJsonFormat
}

object TDouble extends TDouble {
  override val t_memo_ : String = "Double"
  def apply(code:String,memo:String) = new TDouble{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}

trait TFloat extends TMono[Float] {
  val vtm = classTag[Float]
  def parse(str: String): Pack = Pack(str.toFloat)
  implicit val valueFormat = TFloatJsonFormat
}

object TFloat extends TFloat {
  override val t_memo_ : String = "Float"
  def apply(code:String,memo:String) = new TFloat{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}

trait TBigDecimal extends TMono[BigDecimal] {
  val vtm = classTag[BigDecimal]
  def parse(str: String): Pack = Pack(BigDecimal.apply(str))
  implicit val valueFormat = TBigDecimalJsonFormat
}

object TBigDecimal extends TBigDecimal {
  override val t_memo_ : String = "BigDecimal"
  def apply(code:String,memo:String) = new TBigDecimal{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }
}


import com.github.nscala_time.time.Imports._


trait TDateTime extends TMono[DateTime] {
  val vtm = classTag[DateTime]

  def parse(str: String): Pack = string2DatePack(str)

  def parse(str: String, pattern: String): Pack = Pack(DateTimeFormat.forPattern(pattern).parseDateTime(str))

  def format(dt: Pack)(implicit pattern: String = "YYYY-MM-dd HH:mm:ss"): String = dt.value.toString(pattern)

  override protected def packToString(i: ValuePack[DateTime]): String = i.value.toString("YYYY-MM-dd HH:mm:ss")

  implicit def dataPack2DateTime(i: Pack): Imports.DateTime = i.value


  implicit def dataPack2String(i: Pack)(implicit pattern: String = "YYYY-MM-dd HH:mm:ss"): String = format(i)(pattern)


  implicit def string2DatePack(dateTimeStr: String)(implicit pattern: String = "YYYY-MM-dd HH:mm:ss"): Pack = {

    TDateTime.string2DateTime(dateTimeStr) match {
      case Some(v) => Pack(v)
      case None => throw new IllegalArgumentException("Invalid date time format: \"" + dateTimeStr + '"')
    }
  }
  implicit val valueFormat = DateTimeJsonFormat
}

object TDateTime extends TDateTime {
  override val t_memo_ : String = "DateTime"
  def apply(code:String,memo:String) = new TDateTime{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }

  def string2DateTime(dateTimeStr: String)(implicit pattern: String = "YYYY-MM-dd HH:mm:ss"): Option[DateTime] = {

    val patterns = pattern ::
      "YYYY-MM-dd HH:mm:ss" ::
      "YYYY-MM-dd HH:mm:ss.SSS" ::
      "YYYY-MM-dd" ::
      "HH:mm:ss" ::
      "HH:mm:ss.SSSZZ" ::
      "HH:mm:ssZZ" ::
      "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
      "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
      Nil

    val init: Option[DateTime] = None


    def tryParse(res: Option[DateTime], pt: String) = {
      if (res.isDefined) res
      else try {
        Some(DateTimeFormat.forPattern(pt).parseDateTime(dateTimeStr))
      } catch {
        case ex: Throwable => None
      }
    }

    (init /: patterns)(tryParse)
  }

   def date2String(date: DateTime)(implicit pattern: String = "YYYY-MM-dd HH:mm:ss") = date.toString(pattern)
}

trait TBoolean extends TMono[Boolean] {
  val vtm = classTag[Boolean]

  def YesTexts = List("true", "True", "TRUE", "Y", "y", "YES", "yes", "1", "是", "有","真")

  def NoTexts = List("false", "False", "FALSE", "N", "n", "No", "no", "0", "否", "无", "非", "空", "假","")

  val YesName = "true"

  val NoName = "false"

  def parse(str: String): Pack = string2BoolPack(str)

  implicit def string2BoolPack(str: String): Pack =
    if (str == YesName || YesTexts.contains(str)) Pack(true)
    else if (str == NoName || NoTexts.contains(str) || str == NoName) Pack(false)
    else throw new IllegalArgumentException("Invalid boolean text: \"" + str + '"')

  implicit def Int2BoolPack(i: Int): Pack =
    if (i != 0) Pack(true)
    else Pack(false)

  implicit def string2BoolValue(str: String): Boolean =
    if (str == YesName || YesTexts.contains(str)) true
    else if (str == NoName || NoTexts.contains(str)) false
    else throw new IllegalArgumentException("Invalid boolean text: \"" + str + '"')

  implicit def boolPack2Bool(i: Pack): Boolean = i.value

  implicit def boolPack2String(i: Pack): String = if (i.value) YesName else NoName

  override protected def packToString(i: ValuePack[Boolean]): String = if (i.value) YesName else NoName
  implicit val valueFormat = TBooleanJsonFormat
}

object TBoolean extends TBoolean {
  override val t_memo_ : String = "Boolean"
  def apply(code:String,memo:String) = new TBoolean{
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }

  def apply(code:String,memo:String,yesText:String,noText:String) = new TBoolean{
    override val t_memo_ = memo
    override lazy val t_code_ = code
    override val YesName = yesText
    override val NoName = noText
  }
}


case class EnumIdx(idx: Int)

trait TEnum extends TMono[EnumIdx] {
  this: Enumeration =>

  val vtm = classTag[EnumIdx]

  EnumRegistry.register(getClass.getName.replace("$", ""), this)

  override protected def packToString(i: ValuePack[EnumIdx]): String = i.value.idx.toString

  override def parse(str: String): Pack = name2EnumPack(str)

  implicit def int2EnumValue(id: Int): TEnum.this.type#Value = this.apply(id)

  implicit def name2EnumValue(name: String): TEnum.this.type#Value = this.withName(name)

  implicit def int2EnumPack(id: Int): Pack = Pack(EnumIdx(id))

  implicit def name2EnumPack(name: String): Pack = Pack(EnumIdx(this.withName(name).id))

  implicit def enumValue2Int(ev: this.type#Value): Int = ev.id

  implicit def enumValue2Name(ev: this.type#Value): String = ev.toString

  implicit def int2Name(id: Int): String = this.apply(id).toString

  implicit def enumValue2Pack(ev: this.type#Value): TEnum.this.type#Pack = Pack(EnumIdx(ev.id))

  //implicit def Packe2EnumValue(ev: this.type#Value) = Pack(EnumIdx(ev.id))

  implicit def enumPack2Int(ei: Pack): Int = ei.value.idx

  implicit def enumPack2Name(ei: Pack): String = this.apply(ei.value.idx).toString

  implicit def enumPack2EnumValue(ei: Pack): (TEnum with Enumeration)#Value = this(ei.value.idx)

  implicit def EnumIdx2EnumValue(idx: EnumIdx): (TEnum with Enumeration)#Value = this(idx.idx)

  implicit def EnumValue2EnumIdx(ev: this.type#Value): EnumIdx = EnumIdx(ev.id)

  implicit val valueFormat = EnumIdxFormat

  implicit object EnumIdxFormat extends JsonFormat[EnumIdx] {
    def write(x: EnumIdx) = JsObject("idx" -> JsNumber(x.idx), "name" -> JsString(int2Name(x.idx)))

    def read(value: JsValue) = value match {
      case JsNumber(x) if x.intValue() > 0 && x.intValue() < maxId => int2EnumPack(x.intValue())
      case JsNumber(x) => deserializationError(s"Expected enum value in 0 .. $maxId, but got " + x)
      case JsString(x) if values.exists(_.toString == x) => name2EnumPack(x)
      case JsString(x) if isIntStr(x) =>
        val x2 = x.toInt
        if (x2 > 0 && x2 < maxId) int2EnumPack(x2)
        else deserializationError(s" $x is not a allow value")
      case JsString(x) => deserializationError(s" $x is not a allow value")
      case x: JsObject if x.fields.contains("idx") => EnumIdx(x.fields("idx").convertTo[Int])
      case x => deserializationError("Expected enum value as JsString or JsNumber, but got " + x)
    }
  }

  private def isIntStr(s: String) = try {
    s.toInt
    true
  } catch {
    case _: Throwable => false
  }

  override def AnyToPack(v: Any): Option[ValuePack[EnumIdx]] = {
    super.AnyToPack(v) match {
      case Some(p: ValuePack[_]) =>
        if (p.value.idx >= maxId) None
        else Some(p)

      case None =>
        v match {
          case num: Int if num < maxId => Some(int2EnumPack(num))
          case txt: String if values.exists(_.toString == txt) => Some(name2EnumPack(txt))
          case _ => None
        }
    }

  }

  override def typeInfo: EnumTypeInfo =
    new EnumTypeInfo(getClass.getName.replace("$", ""), t_memo_,
      values.toList.map(v => (v.id, v.toString)))
}

object TEnum extends Enumeration with TEnum {
  override val t_memo_ : String = "Enum"
}

object EnumRegistry {
  //保存所有的 TStruct 实例,以 typeName 为key
  private[this] val _registry = new AtomicReference(Map.empty[String, TEnum])

  @tailrec
  def register(key: String, dt: TEnum): TEnum = {
    val reg = _registry.get
    val updated = reg.updated(key, dt)
    if (_registry.compareAndSet(reg, updated)) dt
    else register(key, dt)
  }

  def get(key: String): Option[TEnum] = _registry.get.get(key)

  def all =_registry.get.map(_._2).toList
}
