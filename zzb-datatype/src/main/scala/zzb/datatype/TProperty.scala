package zzb.datatype

import spray.json.{JsValue, JsonFormat}
import zzb.datatype.meta.{MapTypeInfo, TypeInfo}
import scala.collection.Iterable
import scala.reflect._

/**
 * Created by Simon on 2014/6/27
 */
//class TProperty extends DataType[Map[String,TVariant.Pack]]{
//  override val vtm: ClassTag[_] = _
//
//  override def fromJsValue(x: JsValue): ValuePack[Map[String, TVariant.Pack]] = ???
//
//  override def AnyToPack(v: Any): Option[ValuePack[Map[String, TVariant.Pack]]] = ???
//
//  override def typeInfo: TypeInfo = ???
//
//  override type Pack = this.type
//  override val t_memo_ : String = _
//}


trait TProperty extends TStrKeyPackMap[TVariant.Pack] {

  import BasicFormats._

  override def valueDataType = TVariant

  override val km = classTag[String]
  override val vm = classTag[TVariant.Pack]
  override implicit val keyFormat = StringJsonFormat
  override implicit val valueFormat: JsonFormat[TVariant.Pack] = TVariant.Pack.packJsonFormat

  val empty: TProperty#Pack = this(Map[String,TVariant.Pack]())
  val thisType = this

  implicit class PackWrap(p: Pack) extends Iterable[(String, TVariant.Pack)] {

    override def size = p.size

    def get(key: String) = p.value.get(key)

    def iterator = p.value.iterator

    override def isEmpty: Boolean = size == 0

    def getOrElse(key: String, default: => TVariant.Pack) = p.value.getOrElse(key, default)

    def apply(key: String) = p.value.apply(key)

    def contains(key: String) = p.value.contains(key)

    def isDefinedAt(key: String) = contains(key)

    def keySet = p.value.keySet

    def keysIterator = p.value.keysIterator

    def keys = p.value.keys

    def values = p.value.values

    def valuesIterator = p.value.valuesIterator

    def +(kv: (String, TVariant.Pack)) = thisType(p.value + kv)

    def -(key: String) = thisType(p.value - key)

    def filterKeys(kf: String => Boolean) = thisType(p.value.filterKeys(kf))

    def mapValues[C](f: TVariant.Pack => C) = p.value.mapValues(f)

    def updated(key: String, value: TVariant.Pack) = this + ((key, value))

  }
}

object TProperty extends TProperty{
  override val t_memo_ : String = "Property"

  def apply(code:String,memo:String) = new TProperty {
    override val t_memo_ : String = memo
    override lazy val t_code_ : String = code
  }
}