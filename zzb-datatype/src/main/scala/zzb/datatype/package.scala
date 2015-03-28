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
  implicit class TListFieldTrans[T<:TList[_]](val field: () => T) {
    def :=[U](value: List[U])(implicit um:ClassTag[U]) = {
      if(field().lm != um) throw new IllegalArgumentException
      Some(field().applyListValue(value) )
    }
  }

  //TProperty 字段赋值的 “:= 语法”
  implicit class TPropertyFieldTrans[T<:TProperty](val field: () => T ) {
    def :=(value: Map[String,Any]) = Some(
      field().applyMapValue(value.map(kv => (kv._1,TVariant(kv._2.toString))))
    )
  }


  //TMap 字段赋值的 “:= 语法”
  implicit class TMapFieldTrans[T<:TMap[_,_]](val field: () => T) {
    def :=[K,V](value: Map[K,V])(implicit km:ClassTag[K], vm:ClassTag[V]) = {
      val (lkm,lvm) = (field().km,field().vm)
      if(lkm != km || lvm != vm)
        throw new IllegalArgumentException
      //val ddd: TMap.this.type#Pack = field().applyMapValue(value)
      Some(field().applyMapValue(value))
    }
  }

  // 字段赋值的 “:= 语法”
  implicit class TStructFieldTrans[T<: TStruct](val field: () => T) {
    def :=(value: T#Pack) = Some(field().apply(value))
  }

  // 字段赋值的 “:= 语法”
  implicit class TStringFieldTrans(val field: () => TString) {
    def :=(value: String) = Some(field().apply(value))
  }

  // 字段赋值的 “:= 语法”
  implicit class TIntFieldTrans(val field: () => TInt) {
    def :=(value: Int) = Some(field().apply(value)  )
  }

  implicit class TLongFieldTrans(val field: () => TLong) {
    def :=(value: Long) = Some(field().apply(value))
  }

  implicit class TByteFieldTrans(val field: () => TByte) {
    def :=(value: Byte) = Some(field().apply(value))
  }

  implicit class TShortFieldTrans(val field: () => TShort) {
    def :=(value: Short) = Some(field().apply(value))
  }

  implicit class TFloatFieldTrans(val field: () => TFloat) {
    def :=(value: Float) = field().apply(value)
  }

  implicit class TDoubleFieldTrans(val field: () => TDouble) {
    def :=(value: Double) = Some(field().apply(value))
  }

  import com.github.nscala_time.time.Imports._

  implicit class TDateTimeFieldTrans(val field: () => TDateTime) {
    def :=(value: DateTime) = Some(field().apply(value))
  }

  implicit class TBigDecimalFieldTrans(val field: () => TBigDecimal) {
    def :=(value: BigDecimal) = Some(field().apply(value) )
  }

  implicit class TBooleanFieldTrans(val field: () => TBoolean) {
    def :=(value: Boolean) = Some(field().apply(value) )
  }

  private val datePatterns ="YYYY-MM-dd HH:mm:ss" ::
  "YYYY-MM-dd" ::
    "HH:mm:ss" ::
    "HH:mm:ss.SSSZZ" ::
    "HH:mm:ssZZ" ::
    "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
    "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" ::
    "YYYY-MM-dd HH:mm:ss.SSS" ::
    Nil

  private def tryParseDate(str:String)(res: Option[DateTime], pt: String) = {
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
        (init /: datePatterns)(tryParseDate(dateStr)) match{
          case Some(v) => v
          case None => throw new IllegalArgumentException("Invalid date time format: \"" + dateStr + '"')
        }
      case x => deserializationError("Expected datetime as JsString, but got " + x)
    }
  }
}
