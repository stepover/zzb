package zzb.datatype

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicReference

import spray.json._
import zzb.datatype.meta.TypeInfo
import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午5:07
 * Copyright baoxian.com 2012~2020
 */
trait TMono[VT] extends DataType[VT] {

  protected def packToString(i: ValuePack[VT]): String = i.value.toString

  implicit def value2Pack(v: VT) = Pack(v)

  implicit def Pack2Value(v: Pack): VT = v.value

  //def apply[VT2](v: VT2): ValuePack[VT2] = Pack(v)

  def apply(v: VT): Pack = Pack(v)

  def apply(i: Pack) = Pack(i.value)

  def unapply(v: Pack) = if (v == null) None else Some(v.value)


  implicit val valueFormat: JsonFormat[VT]


  override def typeInfo: TypeInfo = new TypeInfo(vtm.runtimeClass.getSimpleName, t_memo_)

  MonoRegistry.register(getClass.getName + ":" + t_code_, this.asInstanceOf[TMono[Any]])

  override def AnyToPack(v: Any): Option[ValuePack[VT]] = v match {
    case p: Pack => Some(p)
    case p: ValuePack[_] =>
      if (boxedType(vtm.runtimeClass).isInstance(p.value))
        Some(Pack(p.value.asInstanceOf[VT]))
      else None
    case mv =>
      if (boxedType(vtm.runtimeClass).isInstance(v))
        Some(Pack(mv.asInstanceOf[VT]))
      else None
  }

  def parse(str: String): Pack

  def validators: List[Pack => Option[String]] = List()

  //此类型和下面的隐式转换构成 字段赋值的 “:= 语法”
  implicit class FieldFuncWrap(val field: () => this.type) {
    def :=(value: VT) = Some(field().apply(value))

    def :=(value: Option[VT]) = value match {
      case Some(v:VT) => Some(field().apply(v.value))
      case _ => None
    }
  }

  def fromJsValue(x: JsValue): Pack = Pack(valueFormat.read(x))

  implicit object Format extends JsonFormat[Pack] {
    def read(json: JsValue): Pack = {
      fromJsValue(json)
    }

    def write(pack: Pack) = pack.toJsValue
  }

  case class Pack(value: VT)
    extends {
      val dataType = TMono.this

    } with ValuePack[VT]  with Serializable{
    //type vType = valueType


    //----------------------------序列化 begin
    private def writeObject(os: ObjectOutputStream): Unit = {
      //      val clazz = this.dataType.me.getClass.getName
      val meta = this.dataType.me.getClass.getName + ":" + dataType.t_code_
      val data = json
      val sd = meta + "~~~" + data
      os.writeUTF(sd)
    }


    private def readObject(is: ObjectInputStream) = readObjectDo(is)

    private def readResolve: AnyRef = {
      __sSerialData match {
        case meta :: data :: Nil =>
          val jv = JsonParser(data)
          MonoRegistry.get(meta) match {
            case Some(d) =>
              val oo = d.fromJsValue(jv)
              oo
            case None =>
              //调用一下数据类型，使其注册到MonoRegistry中
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
                  MonoRegistry.get(meta) match {
                    case Some(d) =>
                      val oo = d.fromJsValue(jv)
                      oo
                    case None => throw new IllegalArgumentException(s"未注册的类型：$meta")
                  }
              }
          }
        case _ => throw new IllegalArgumentException(s"反序列化无法解析：${__sSerialData}")
      }
    }

    //----------------------------序列化 end


    def validate: List[String] = validate(validators)

    override def toString = packToString(this)

    override def toJsValue: JsValue = valueFormat.write(value)
  }

  object Pack extends DefaultJsonProtocol {
    implicit val packJsonFormat = new RootJsonFormat[Pack] {
      override def read(json: JsValue): Pack = fromJsValue(json)

      override def write(obj: Pack): JsValue = obj.toJsValue
    }
  }
}

object TMono {
  def apply[T](code: String, memo: String)(implicit format: JsonFormat[T], parser: String => T, m: ClassTag[T]) = new TMono[T] {
    override val t_memo_ = memo
    override lazy val t_code_ = code
    override implicit val valueFormat = format

    override def parse(str: String): Pack = Pack(parse(str))

    override val vtm = m
  }
}


object MonoRegistry {
  //保存所有的 TStruct 实例,以 typeName 为key
  private[this] val _registry = new AtomicReference(Map.empty[String, TMono[Any]])

  @tailrec
  def register(key: String, dt: TMono[Any]): TMono[Any] = {
    val reg = _registry.get
    val updated = reg.updated(key, dt)
    if (_registry.compareAndSet(reg, updated)) dt
    else register(key, dt)
  }

  def get(key: String): Option[TMono[Any]] = _registry.get.get(key)

  def all = _registry.get.map(_._2).toList.sortWith((s1, s2) => s1.typeInfo.valueCode.compareTo(s2.typeInfo.valueCode) < 0)

}

