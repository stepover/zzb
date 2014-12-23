package zzb.datatype

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce
import spray.json._
import scala.reflect._
import zzb.datatype.meta.{ListTypeInfo, TypeInfo}

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午5:08
 * Copyright baoxian.com 2012~2020
 */

trait ListPack[T] extends ValuePack[List[T]] {
  def length: Int

  //def values: List[Any]

}

trait TList[T] extends DataType[List[T]] {

  val vtm = classTag[List[T]]
  val lm: ClassTag[_]

  def topDoc = false

  override def typeInfo: TypeInfo = new ListTypeInfo(lm.runtimeClass.getSimpleName, t_memo_)

  ListRegistry.register(getClass.getName + ":" + t_code_,this.asInstanceOf[TList[Any]])

  override def AnyToPack(v: Any) = v match {
    case p: Pack => Some(p)
    case list: List[_] if list.size == 0 => Some(Pack(Nil))
    case list: List[_] if lm.runtimeClass.isInstance(list.head) =>
      Some(Pack(list.asInstanceOf[List[T]]))
    case _ => None
  }

  implicit def value2Pack(v: List[T]) = Pack(v)

  implicit val elementFormat: JsonFormat[T]

  def apply(v: List[T]): Pack = Pack(v)
  def applyListValue(v: List[_]): Pack =
    Pack(v.asInstanceOf[List[T]])

  def apply(i: Pack) = Pack(i.value)

  //todo:
  def parse(str: String) = Pack(List[T]())


  protected def packToString(p: Pack) = p.value.toString()

  //此类型和下面的隐式转换构成 字段赋值的 “:= 语法”
  implicit class FieldTrans(val field: () => this.type) {
    def :=(value: List[T]) = field().apply(value)
  }

  def validators: List[Pack => Option[String]] = List()

  def fromJsValue(x: JsValue): Pack = Format.read(x)

  implicit object Format extends JsonFormat[Pack] {

    import spray.json._

    def read(json: JsValue): Pack = json match {
      case JsArray(elements) =>
        val values = elements.map(e => elementFormat.read(e)).toList
        Pack(values)
      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }

    def write(pack: Pack): JsValue = pack.toJsValue
  }

  case class Pack(value: List[T])
    extends {
      val dataType = TList.this
    } with ListPack[T]  with Serializable{

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
          ListRegistry.get(meta) match {
            case Some(d) =>
              val oo = d.fromJsValue(jv)
              oo
            case None =>
              //调用一下数据类型，使其注册到ListRegistry中
              try {
                val mp = meta.substring(0,meta.indexOf(":"))
                val c = if (mp.endsWith("$")) Class.forName(mp) else Class.forName(mp + "$")
                val module = c.getDeclaredField("MODULE$")
                module.setAccessible(true)
                val oo = module.get(null)
                val data = c.getMethod("fromJsValue", classOf[JsValue]).invoke(oo, jv)
                data
              } catch {
                case e: Throwable =>
                  ListRegistry.get(meta) match {
                    case Some(d) =>
                      val oo = d.fromJsValue(jv)
                      oo
                    case None => throw new IllegalArgumentException(s"未注册的TList类型：$meta")
                  }
              }
          }
        case _ => throw new IllegalArgumentException(s"反序列化无法解析TList：${__sSerialData})")
      }
    }
    //----------------------------序列化 end


    def length: Int = value.length

    //override def toString = packToString(this)

    def apply(n: Int): T = value(n)

    def head: T = value.head

    def tail = value.tail

    // Adds an element at the beginning of this list.
    def ::(x: T): Pack = Pack(x :: value)

    // Adds the elements of a given list in front of this list.
    def :::(prefix: List[T]): Pack = Pack(prefix ::: value)

    def addItem(item: Any) =
      if (lm.runtimeClass.isInstance(item))
        Pack((item.asInstanceOf[T] :: value.reverse).reverse)
      else{ //对基本类型的支持
        item match {
          case p :TMono[_]#Pack if p.dataType.vtm == lm  =>
            Pack((p.value.asInstanceOf[T] :: value.reverse).reverse)
          case _ => throw new IllegalArgumentException
        }
      }

    def setItem(idx: Int, item: Any) = {
      if (idx < 0 || idx >= length)
        throw new IndexOutOfBoundsException
      else if (lm.runtimeClass.isInstance(item)) {
        var i = -1
        Pack(value.map {
          it =>
            i = i + 1
            if (i == idx) item.asInstanceOf[T]
            else it
        })
      }
      else
        throw new IllegalArgumentException
    }

    def getItem(idx:Int) ={
      if (idx < 0 || idx >= length)
        throw new IndexOutOfBoundsException
      value.take(idx+1).reverse.head
    }
    def removeItem(idx:Int) = {
      if (idx < 0 || idx >= length)
        throw new IndexOutOfBoundsException
      var i = -1
      Pack(value.filter{
        it =>
          i = i + 1
          i != idx
      })
    }

    def ++(that: Pack) = Pack(value ++ that.value)

    def ++(that: List[T]) = Pack(value ++ that)

    def map[B](f: T => B) = value.map(f)

    def flatMap[B](f: T => GenTraversableOnce[B]) = value.flatMap(f)

    def filter(p: T => Boolean) = value.filter(p)

    def foreach[B](f: T => B) = value.foreach(f)

    def forall(p: T => Boolean): Boolean = value.forall(p)

    def foldLeft[B](z: B)(f: (B, T) => B): B = value.foldLeft(z)(f)

    def foldRight[B](z: B)(f: (T, B) => B): B = value.foldRight(z)(f)

    def reduceLeft[B >: T](f: (B, T) => B): B = value.reduceLeft(f)

    def reduceRight[B >: T](op: (T, B) => B): B = value.reduceRight(op)

    def withFilter(p: T => Boolean) = value.withFilter(p)

    def validate: List[String] = validate(validators)

    override def toJsValue: JsValue = JsArray(value.map(_.toJson))

  }

}

object TList{
  def apply[T](code:String,memo:String)(implicit format:JsonFormat[T],m: ClassTag[T]) = new TList[T] {
    override lazy val t_code_ = code
    override val t_memo_ = memo
    override implicit val elementFormat = format
    override val lm = m
  }
}

trait TPackList[I <: ValuePack[Any]] extends TList[I] {
  def itemDataType: DataType[Any]
  override def typeInfo: TypeInfo = new ListTypeInfo(itemDataType.getClass.getName.replace("$", ""), t_memo_)
}

object TPackList{
  def apply[I <: DataType[_]](dt:I,code:String,memo:String)(implicit format:JsonFormat[I#Pack],mt: ClassTag[I],mv: ClassTag[I#Pack]) =
    new TPackList[I#Pack] {
      override lazy val t_code_ = code
      override val t_memo_ = memo
      override implicit val elementFormat = format
      override val lm = mv
      //override def typeInfo: TypeInfo = new ListTypeInfo(dt.getClass.getName.replace("$", ""), t_memo_)

      override def itemDataType: DataType[_] = dt
    }
}


object ListRegistry {
  //保存所有的 TStruct 实例,以 typeName 为key
  private[this] val _registry = new AtomicReference(Map.empty[String, TList[Any]])

  @tailrec
  def register(key: String, dt: TList[Any]): TList[Any] = {
    val reg = _registry.get
    val updated = reg.updated(key , dt)
    if (_registry.compareAndSet(reg, updated)) dt
    else register(key, dt)
  }

  def get(key: String): Option[TList[Any]] = _registry.get.get(key)

  def all = _registry.get.map(_._2).toList.sortWith((s1, s2) => s1.typeInfo.valueCode.compareTo(s2.typeInfo.valueCode) < 0)

}