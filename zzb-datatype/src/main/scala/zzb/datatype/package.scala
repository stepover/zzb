package zzb

import spray.json._
import scala.reflect.ClassTag
import scala.language.implicitConversions

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-22
 * Time: 上午8:59
 * Copyright baoxian.com 2012~2020
 */
package object datatype {

  type StructField = () => DataType[Any]

  type NestedStructFields = List[StructField]

  implicit class InStructPathThrough(path: NestedStructFields) {
    def through = path.map(p => p()).reverse.head
  }

  implicit def inStructPath2String(path: NestedStructFields): String = "/" + path.map(s => s().t_code_).mkString("/")

  implicit def dt2InStructPath(dt: DataType[Any]): NestedStructFields = dt.path

  implicit def structField2InStructPath(field: StructField): NestedStructFields = field().path

  def boxedType(rtClass: Class[_]) = rtClass match {
    case java.lang.Byte.TYPE => Class.forName("java.lang.Byte")
    case java.lang.Short.TYPE => Class.forName("java.lang.Short")
    case java.lang.Character.TYPE => Class.forName("java.lang.Character")
    case java.lang.Integer.TYPE => Class.forName("java.lang.Integer")
    case java.lang.Long.TYPE => Class.forName("java.lang.Long")
    case java.lang.Float.TYPE => Class.forName("java.lang.Float")
    case java.lang.Double.TYPE => Class.forName("java.lang.Double")
    case java.lang.Boolean.TYPE => Class.forName("java.lang.Boolean")
    case rtc => rtc
  }

  implicit class ValuePackOptWarp(itemOpt: Option[ValuePack[Any]]) {
    def apply[VT](dt: DataType[VT]): Option[ValuePack[VT]] = itemOpt match {
      case None =>
        //        if (dt._debug)
        //          println(s"not try to find ${dt.t_code_} from none")
        None

      case Some(item) =>
        //        if (dt._debug)
        //          println(s"try to find ${dt.t_code_} from ${item.dataType.t_code_}")
        item(dt)
    }

    def <~:[VT](v: Option[ValuePack[VT]]): Option[ValuePack[VT]] = itemOpt match {
      case None => v
      case Some(p) =>
        v match {
          case None => None
          case Some(c) => Some(c.plus(p))
        }
    }


    def <~:[VT](v: ValuePack[VT]): ValuePack[VT] = itemOpt match {
      case None => v
      case Some(p) => v plus p
    }


    def <~:[T <: TStruct](v: T#Pack): T#Pack = itemOpt match {
      case None => v
      case Some(p) => v plus p
    }

  }

  implicit class ValuePackList(list: Seq[ValuePack[Any]]) {
    def <~:[VT](v: Option[ValuePack[VT]]): Option[ValuePack[VT]] = {
      v match {
        case None => None
        case Some(c) => Some(c.plusList(list))
      }
    }

    def <~:[VT](v: ValuePack[VT]): ValuePack[VT] = {
      v.plusList(list)
    }
  }

  //TList 字段赋值的 “:= 语法”
  implicit class TListFieldTrans[T <: TList[_]](val field: () => T) {
    def :=[U](value: List[U])(implicit um: ClassTag[U]) = {
      if (field().lm != um) throw new IllegalArgumentException
      Some(field().applyListValue(value))
    }

    def :=[U](value: Option[List[U]])(implicit um: ClassTag[U]) = value match {
      case Some(v) =>
        if (field().lm != um) throw new IllegalArgumentException
        Some(field().applyListValue(v))
      case None => None
    }
  }

  //TProperty 字段赋值的 “:= 语法”
  implicit class TPropertyFieldTrans[T <: TProperty](val field: () => T) {
    def :=(value: Map[String, Any]) = Some(
      field().applyMapValue(value.map(kv => (kv._1, TVariant(kv._2.toString))))
    )

    def :=(value: Option[Map[String, Any]]) = value match {
      case Some(v) => Some(
        field().applyMapValue(v.map(kv => (kv._1, TVariant(kv._2.toString))))
      )
      case None => None
    }
  }


  //TMap 字段赋值的 “:= 语法”
  implicit class TMapFieldTrans[T <: TMap[_, _]](val field: () => T) {
    def :=[K, V](value: Map[K, V])(implicit km: ClassTag[K], vm: ClassTag[V]) = {
      val (lkm, lvm) = (field().km, field().vm)
      if (lkm != km || lvm != vm)
        throw new IllegalArgumentException
      Some(field().applyMapValue(value))
    }

    def :=[K, V](value: Option[Map[K, V]])(implicit km: ClassTag[K], vm: ClassTag[V]) = value match {
      case Some(v) =>
        val (lkm, lvm) = (field().km, field().vm)
        if (lkm != km || lvm != vm)
          throw new IllegalArgumentException
        Some(field().applyMapValue(v))
      case None => None
    }
  }

  // 字段赋值的 “:= 语法”
  implicit class TStructFieldTrans[T <: TStruct](val field: () => T) {
    def :=(value: T#Pack) = Some(field().apply(value))

    def :=(value: Option[T#Pack]) = value match {
      case Some(v) => Some(field().apply(v))
      case None => None
    }
  }

  class MonoTrans[VT,DT<:TMono[VT]]( field: () => DT)(implicit m: ClassTag[VT]){
    def :=(value: VT) = Some(field().apply(value))
    def :=(value: Option[_]) = value match {
      case Some(v) if boxedType(m.runtimeClass).isInstance(v)  => Some(field().apply(v.asInstanceOf[VT]))
      case Some(v:DT#Pack) if v.value != null  => Some(field().apply(v.value))
      case None => None
    }
    def :=(value: DT#Pack) = Some(field().apply(value.value))
  }

  implicit class TStringFieldTrans(field: () => TString) extends MonoTrans[String,TString](field)

  implicit class TIntFieldTrans(field: () => TInt) extends MonoTrans[Int,TInt](field)

  implicit class TLongFieldTrans(field: () => TLong) extends MonoTrans[Long,TLong](field)

  implicit class TByteFieldTrans(field: () => TByte) extends MonoTrans[Byte,TByte](field)

  implicit class TShortFieldTrans(field: () => TShort) extends MonoTrans[Short,TShort](field)

  implicit class TFloatFieldTrans(field: () => TFloat) extends MonoTrans[Float,TFloat](field)

  implicit class TDoubleFieldTrans(field: () => TDouble) extends MonoTrans[Double,TDouble](field)

  implicit class TBooleanFieldTrans(field: () => TBoolean) extends MonoTrans[Boolean,TBoolean](field)

  implicit class TBigDecimalFieldTrans(field: () => TBigDecimal) extends MonoTrans[BigDecimal,TBigDecimal](field)

  implicit class TEnumFieldTrans(field: () => TEnum) extends MonoTrans[EnumIdx,TEnum](field)

  import com.github.nscala_time.time.Imports._
  implicit class TDateTimeFieldTrans(field: () => TDateTime) extends MonoTrans[DateTime,TDateTime](field)
  

  private val datePatterns = "YYYY-MM-dd HH:mm:ss" ::
    "YYYY-MM-dd" ::
    "HH:mm:ss" ::
    "HH:mm:ss.SSSZZ" ::
    "HH:mm:ssZZ" ::
    "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
    "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
    "YYYY-MM-dd HH:mm:ss.SSS" ::
    Nil

  private def tryParseDate(str: String)(res: Option[DateTime], pt: String) = {
    if (res.isDefined) res
    else try {
      Some(DateTimeFormat.forPattern(pt).parseDateTime(str))
    } catch {
      case ex: Throwable => None
    }
  }

  implicit class String2DateTimeTrans(val s: String) {
    def toDateTime(implicit pattern: String = "YYYY-MM-dd HH:mm:ss") = {
      val patterns = pattern :: datePatterns

      val init: Option[DateTime] = None

      val res: Option[DateTime] = (init /: patterns)(tryParseDate(s))

      res match {
        case Some(v) => v
        case None => throw new IllegalArgumentException("Invalid date time format: \"" + s + '"')
      }
    }
  }

  implicit object DateTimeJsonFormat extends JsonFormat[DateTime] {
    def write(date: DateTime) = JsString(date.toString("YYYY-MM-dd HH:mm:ss.SSS"))

    def read(value: JsValue) = value match {
      case JsString(dateStr) =>
        val init: Option[DateTime] = None
        (init /: datePatterns)(tryParseDate(dateStr)) match {
          case Some(v) => v
          case None => throw new IllegalArgumentException("Invalid date time format: \"" + dateStr + '"')
        }
      case x => deserializationError("Expected datetime as JsString, but got " + x)
    }
  }

}
