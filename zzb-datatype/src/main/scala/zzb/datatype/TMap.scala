package zzb.datatype

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicReference

import spray.json._
import zzb.datatype.meta.{MapTypeInfo, TypeInfo}

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import scala.reflect._

/**
 * Created by Simon on 2014/4/17
 */
trait TMap[K, V] extends DataType[Map[K, V]] {

  type ItemFilter = ((K, V)) => Boolean

  val vtm = classTag[Map[K, V]]

  val km: ClassTag[_]
  val vm: ClassTag[_]

  override def AnyToPack(v: Any) = v match {
    case p: Pack => Some(p)
    case _ => None
  }

  def topDoc = false

  override def typeInfo: TypeInfo = new MapTypeInfo(km.runtimeClass.getName, vm.runtimeClass.getName, t_memo_)

  val itemFilter: ItemFilter = kv => true

  MapRegistry.register(getClass.getName + ":" + t_code_, this.asInstanceOf[TMap[Any, Any]])

  implicit val keyFormat: JsonFormat[K]

  implicit val valueFormat: JsonFormat[V]


  def fromJsValue(jsValue: JsValue): Pack = jsValue match {
    case x: JsObject =>
      val values = x.fields.map {
        case (key, jsv) if itemFilter(JsString(key).convertTo[K], jsv.convertTo[V]) =>
          (JsString(key).convertTo[K], jsv.convertTo[V])
      }.toMap
      Pack(values, kv => true)
    case x => deserializationError("Expected Map as JsObject, but got " + x)
  }

  def apply(v: Map[K, V], filter: ItemFilter = itemFilter): TMap.this.type#Pack = Pack(v.filter(p => filter(p)), filter)

  def applyMapValue(v: Map[_, _]): Pack = apply(v.asInstanceOf[Map[K, V]])


  //def apply(filter:Set[K])(values: (K, V)*) = Pack(Map[K, V](),filter) ++ values

  //此类型和下面的隐式转换构成 字段赋值的 “:= 语法”
  implicit class FieldTrans(val field: () => this.type) {
    def :=(value: Map[K, V]) = field().apply(value)
  }

  def validators: List[Pack => Option[String]] = List()


  case class Pack(value: Map[K, V], oneFilter: ((K, V)) => Boolean) extends {
    val dataType = TMap.this
  } with MapPack[K, V] with Serializable {
    type vType = Map[K, V]


    //----------------------------序列化 begin
    private def writeObject(os: ObjectOutputStream): Unit = {
      val meta = dataType.me.getClass.getName + ":" + dataType.t_code_
      val data = json
      val sd = meta + "~~~" + data
      os.writeUTF(sd)
    }

    private def readObject(is: ObjectInputStream) = readObjectDo(is)

    private def readResolve: AnyRef = {
      __sSerialData match {
        case meta :: data :: Nil =>
          val jv = JsonParser(data)
          MapRegistry.get(meta) match {
            case Some(d) =>
              val oo = d.fromJsValue(jv)
              oo
            case None =>
              //调用一下数据类型，使其注册到MapRegistry中
              try {
                val mp = meta.substring(0, meta.indexOf(":"))
                val c = if (mp.endsWith("$")) Class.forName(mp) else Class.forName(mp + "$")
                val module = c.getDeclaredField("MODULE$")
                module.setAccessible(true)
                val oo = module.get(null)
                val data = c.getMethod("fromJsValue", classOf[JsValue]).invoke(oo, jv)
                data
              } catch {
                case e: Throwable =>
                  MapRegistry.get(meta) match {
                    case Some(d) =>
                      val oo = d.fromJsValue(jv)
                      oo
                    case None => throw new IllegalArgumentException(s"未注册的TMap类型：$meta")
                  }
              }
          }
        case _ => throw new IllegalArgumentException(s"反序列化无法解析TMap：${__sSerialData})")
      }
    }

    //----------------------------序列化 end


    def apply(k: K): Option[V] = get(k)

    def get(k: K): Option[V] = value.get(k)

    value.foreach(kv => if (!oneFilter(kv)) throw new NotAllowKeyException(dataType, kv._1))

    def validate: List[String] = validate(validators)

    def contains(k: K) = value.contains(k)

    def plusWith(kv: (K, V)) = {
      val allow = oneFilter(kv)
      if (allow)
        Pack(value + kv, oneFilter)
      else this
    }

    def plusWithAny(kv: (Any, Any)) =
      if (km.runtimeClass.isInstance(kv._1) && vm.runtimeClass.isInstance(kv._2))
        plusWith((kv._1.asInstanceOf[K], kv._2.asInstanceOf[V]))
      else if (km.runtimeClass.isInstance(kv._1) && vm.runtimeClass.isPrimitive ){
        kv._2 match {//对基本类型的支持
          case p :TMono[_]#Pack if p.dataType.vtm == vm  =>
            plusWith((kv._1.asInstanceOf[K], p.value.asInstanceOf[V]))
          case _ => throw new IllegalArgumentException
        }
      }
      else
        throw new IllegalArgumentException

    def +(kv: (K, V)) = plusWith(kv)

    def plusWithList(xs: GenTraversableOnce[(K, V)]) = (this /: xs.seq)(_ + _)

    def ++(xs: GenTraversableOnce[(K, V)]) = plusWithList(xs)

    def subWith(k: K) = if (contains(k: K)) Pack(value - k, oneFilter) else this

    def -(k: K) = subWith(k)

    def subWithList(xs: GenTraversableOnce[K]) = (this /: xs.seq)(_ - _)

    def --(xs: GenTraversableOnce[K]) = subWithList(xs)

    def size = value.size

    def usedKeys = value.keys

    //def availableKeys = fixKeys

    //def allowKeys(keyFilter: K => Boolean) = fixKeys.filter(keyFilter)

    def values = value.values

    def foreach[U](f: ((K, V)) => U): Unit = value.foreach[U](f)

    def map(f: ((K, V)) => (K, V)): Pack = Pack(value.map(f), oneFilter)

    def filter(f: ((K, V)) => Boolean): Pack = Pack(value.filter(f), f)

    def withFilter(f: ((K, V)) => Boolean) = filter(f)

    import spray.json.DefaultJsonProtocol._

    import BasicFormats._

    override def toJsValue: JsValue = value.toJson

    //覆盖操作
    override def ->>(other: ValuePack[Any]): Pack = {
      if (other.dataType != dataType) throw new RuntimeException("data type miss match")
      val o = other.asInstanceOf[Pack]
      o.plusWithList(this.value)
    }
  }

}

object TMap {
  def apply[K, V](code: String, memo: String)(implicit fk: JsonFormat[K], fv: JsonFormat[V], mk: ClassTag[K], mv: ClassTag[V]) = {
    new TMap[K, V] {
      override val km = mk
      override val vm = mv
      override implicit val keyFormat = fk
      override implicit val valueFormat = fv
      override val t_memo_ = memo
      override lazy val t_code_ = code


    }
  }
}

trait MapPack[K, V] extends ValuePack[Map[K, V]] {
  def size: Int
}


class NotAllowKeyException[K, V](val dt: TMap[K, V],
                                 val invalidKey: K) extends Exception(s"${dt.getClass.getName} not allow key ${invalidKey.toString}")

trait TStrKeyMap[V] extends TMap[String, V]

object TStrKeyMap {

  import zzb.datatype.BasicFormats._

  def apply[V](code: String, memo: String)(implicit fv: JsonFormat[V], mv: ClassTag[V]) = new TStrKeyMap[V] {
    override val km = classTag[String]
    override val vm = mv
    override implicit val keyFormat = StringJsonFormat
    override implicit val valueFormat = fv
    override val t_memo_ = memo
    override lazy val t_code_ = code
  }

}

trait TStrKeyPackMap[V <: ValuePack[_]] extends TStrKeyMap[V] {
  def valueDataType: DataType[Any]

  override def typeInfo: TypeInfo = new MapTypeInfo("String", valueDataType.getClass.getName.replace("$", ""), t_memo_)
}

object TStrKeyPackMap {

  import zzb.datatype.BasicFormats._

  def apply[V <: DataType[_]](dt: V, code: String, memo: String)(implicit fv: JsonFormat[V#Pack], mt: ClassTag[V], mv: ClassTag[V#Pack]) = new TStrKeyPackMap[V#Pack] {
    override def valueDataType: DataType[_] = dt

    override val km = classTag[String]
    override val vm = mv
    override implicit val keyFormat = StringJsonFormat
    override implicit val valueFormat = fv
    override val t_memo_ = memo
    override lazy val t_code_ = code

    //override def typeInfo: TypeInfo = new MapTypeInfo("String",dt.getClass.getName.replace("$", ""), t_memo_)
  }
}


object MapRegistry {
  private[this] val _registry = new AtomicReference(Map.empty[String, TMap[Any, Any]])

  @tailrec
  def register(key: String, dt: TMap[Any, Any]): TMap[Any, Any] = {
    val reg = _registry.get
    val updated = reg.updated(key, dt)
    if (_registry.compareAndSet(reg, updated)) dt
    else register(key, dt)
  }

  def get(key: String): Option[TMap[Any, Any]] = _registry.get.get(key)

  def all = _registry.get.map(_._2).toList.sortWith((s1, s2) => s1.typeInfo.valueCode.compareTo(s2.typeInfo.valueCode) < 0)

}