package zzb.datatype

import java.io.{ObjectInputStream, ObjectOutputStream}

import spray.json.{JsonParser, JsValue}
import zzb.datatype.meta.TypeInfo

import scala.language.experimental.macros

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午4:43
 * Copyright baoxian.com 2012~2020
 */

trait ValuePack[+VT] {
  //----------------------------序列化同样方法  begin
 def writeObjectDo(os: ObjectOutputStream): Unit = {
    val meta = dataType.me.typeInfo.valueCode
    val data = json
    val sd = meta + "~~~" + data
    os.writeUTF(sd)
  }

  var __sSerialData = List.empty[String]

  def readObjectDo(is: ObjectInputStream) = {
    __sSerialData = is.readUTF().split("~~~").toList
  }

  def readResolveDo: AnyRef = {
    __sSerialData match {
      case meta :: data :: Nil =>
        val jv = JsonParser(data)
        StructRegistry.get(meta) match {
          case Some(d) =>
            val oo = d.fromJsValue(jv)
            oo
          case None =>
            //调用一下数据类型，使其注册到StructRegistry中
            try {
              val c = if (meta.endsWith("$")) Class.forName(meta) else Class.forName(meta + "$")
              val module = c.getDeclaredField("MODULE$")
              module.setAccessible(true)
              val oo = module.get(null)
              val data = c.getMethod("fromJsValue", classOf[JsValue]).invoke(oo, jv)
              data
            } catch {
              case e: Throwable =>
                StructRegistry.get(meta) match {
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

  val dataType: DataType[VT]

  val value: VT

  def code = dataType.t_code_

  if (dataType.doPreValidate()) {
    val validRes = validate
    if (validRes.length > 0) throw new DataInitValidException(validRes)
  }

  def withType[VT2](dt: DataType[VT2]): Option[ValuePack[VT2]] =
    if (dt == dataType) Some(this.asInstanceOf[ValuePack[VT2]]) else None

  def apply[VT2](dt: DataType[VT2]): Option[ValuePack[VT2]] = {
    val tls = dt.prePathNode.get()
    if (tls != null) {
      val prePathDataType = tls.asInstanceOf[DataType[Any]]
      if (prePathDataType.t_code_ == dataType.t_code_) {
        //        if(dt._debug){
        //          println(s"get no [${dt.t_code_}] parent is [${prePathDataType.t_code_}] self" )
        //        }
        withType(dt)
      } else {
        //        if (prePathDataType._debug | dt._debug) {
        //          println(s"get [${dt.t_code_}] parent is [${prePathDataType.t_code_}]")
        //        }
        dt.prePathNode.remove()
        ValuePackOptWarp(apply(prePathDataType)).apply(dt)
      }
    } else {
      //      if(dt._debug){
      //        println(s"get no [${dt.t_code_}] parent " )
      //      }
      withType(dt)
    }
  }

  def withCode(code: String): Option[ValuePack[Any]] = {
    val standCode = code.substring(0, 1).toLowerCase + code.substring(1, code.length)
    if (standCode == dataType.t_code_) Some(this) else None
  }

  def validate: List[String]

  def validate(validator: this.type => Option[String]): Option[String] = validator(this)

  def validate(validators: List[this.type => Option[String]]): List[String] = for {
    validator <- validators
    res = validator(this)
    if res.isDefined
  } yield res.get

  def isValidated = validate.length == 0

  def plus[VT2](i: ValuePack[VT2]): ValuePack[VT] = this

  def plusList(vs: Seq[ValuePack[Any]]): ValuePack[VT] = this

  def <~:[VT2](v: Option[ValuePack[VT2]]): Option[ValuePack[VT2]] = v match {
    case None => None
    case Some(c) => Some(c.plus(this))
  }

  def <~:[VT2](v: ValuePack[VT2]): ValuePack[VT2] = v plus this

  def <~:[T <: TStruct](v: T#Pack): T#Pack = v plus this

  //覆盖操作,左边的数据字段会覆盖右边的数据字段
  def ->>(other: ValuePack[Any]): ValuePack[Any] = {
    if (other.dataType != dataType) throw new RuntimeException("data type miss match")
    this
  }

  override def equals(that: Any) = that match {
    case None => false
    case Some(thatPack: ValuePack[Any]) => this == thatPack
    case thatPack: ValuePack[Any] =>
      thatPack.dataType == this.dataType && this.value == thatPack.value
    case _ =>
      this.value.equals(that)
  }

  def toJsValue: JsValue

  override def toString = toJsValue.toString()

  def json = toJsValue.toString()
}

trait DataType[+VT] extends Serializable {

  import scala.reflect._

  val vtm: ClassTag[_]

  type Pack <: ValuePack[_]

  var _debug = false


  lazy val t_code_ : String = {
    val className = getClass.getSimpleName.replace("$", "")
    className.substring(0, 1).toLowerCase + className.substring(1, className.length)
  }

  val t_memo_ : String

  def AnyToPack(v: Any): Option[ValuePack[VT]]

  def fromJsValue(x: JsValue): ValuePack[VT]

  def typeInfo: TypeInfo

  //是否执行前置校验，如果设为 true ,ValuePack 构造时如果所赋值不满足校验规则会抛异常
  def doPreValidate() = false

  val prePathNode = new ThreadLocal[Any]

  def path: NestedStructFields = {
    val tls = prePathNode.get()
    if (tls != null) {
      val prePathDataType = tls.asInstanceOf[TStruct]
      prePathDataType.path ::: List(prePathDataType.getFieldFunc(t_code_).get)
    } else List(() => this)
  }

  def me: this.type = {
    prePathNode.remove()
    this
  }

  override def hashCode = t_code_.hashCode
}

class DataInitValidException(msgs: List[String]) extends Exception(msgs.toString())