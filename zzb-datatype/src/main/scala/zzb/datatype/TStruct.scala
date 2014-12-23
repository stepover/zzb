package zzb.datatype

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.concurrent.atomic.AtomicReference

import spray.json._
import zzb.datatype.meta.{FieldDesc, TypeInfo}

import scala.annotation.tailrec
import scala.reflect._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-19
 * Time: 下午5:08
 * Copyright baoxian.com 2012~2020
 */


trait StructPack[+VT] extends ValuePack[VT] {

  def fieldCount: Int
}

case class
StructValue(values: Map[String, ValuePack[Any]],
            fieldMap: Map[String, DataType[Any]],
            requiredField: Set[DataType[Any]]) {

  //  val messages = for (dt <- requiredField if !values.contains(dt.t_code_)) yield dt.t_code_
  //
  //  if (messages.size > 0) {
  //    val msg = messages.mkString(";")
  //    throw new RequiredFieldNotSetException(s"fields [$msg] is required.")
  //  }


  def apply[VT](dt: DataType[VT]): Option[ValuePack[VT]] = values.get(dt.t_code_) match {
    case None => None
    case Some(v) =>
      Some(v.asInstanceOf[ValuePack[VT]])
  }

  def apply(code: String): Option[ValuePack[Any]] = values.get(code)

  def isOnlyRequireFields: Boolean = {
    val reqSet = requiredField.map(_.t_code_)
    values.keySet.forall(reqSet.contains)
  }

  def hasField(code: String) = values.contains(code)

  def hasField(dt: DataType[Any]) = values.contains(dt.t_code_)

  def size = values.size

  //def dataPack = Pack(this)

  def fields = values.values.toList

  def plus(value: ValuePack[Any]) = {
    if (fieldMap.contains(value.code))
      StructValue(values + (value.code -> value), fieldMap, requiredField)
    else this
  }

  def plusList(vs: Seq[ValuePack[Any]]) =
    StructValue(values ++ vs.filter(v => fieldMap.contains(v.code)).map(fv => fv.code -> fv), fieldMap, requiredField)


  def sub(dt: DataType[Any]) = {
    if (fieldMap.contains(dt.t_code_) && values.contains(dt.t_code_))
      if (requiredField.contains(dt))
        throw new RequiredFieldNotSetException(s"fields [${dt.t_code_}] is required.")
      else
        StructValue(values - dt.t_code_, fieldMap, requiredField)
    else this
  }

  def subList(vts: Seq[DataType[Any]]) = {
    val beDel = vts.filter(vt => fieldMap.contains(vt.t_code_) && values.contains(vt.t_code_))
    if (beDel.isEmpty) this
    else
      StructValue(values -- beDel.map(_.t_code_), fieldMap, requiredField)
  }

  def ->>(other: StructValue) = {
    StructValue(other.values ++ values.toList, fieldMap, requiredField)
  }

  //相同的字段去掉(除非是必填项),一方有一方没有的字段留下，不同的字段保留左边的
  def xor(other: StructValue): StructValue = {
    val selectFields = for {
      (code, dt) <- fieldMap
      lv = values.get(code)
      rv = other.values.get(code)
      v: Option[ValuePack[Any]] = (lv, rv) match {
        case (Some(l), None) => lv
        case (None, Some(r)) => rv
        case (Some(l), Some(r)) if l != r || requiredField.contains(dt) => Some(l)
          (l, r) match {
            case (ll: TStruct#Pack, rr: TStruct#Pack) => Some(ll.dataType(ll.value.xor(rr.value)))
            case _ => Some(l)
          }
        case _ => None
      }
      if v.isDefined
    } yield (code, v.get)

    StructValue(selectFields.toMap, fieldMap, requiredField)

  }

  override def equals(that: Any) = that match {
    case None => false
    case thatCompound: StructValue =>
      this.values.equals(thatCompound.values)
    case _ =>
      false
  }

  override def toString = values.toString()
}


trait TStruct extends DataType[StructValue] {

  val vtm = classTag[StructValue]

  type fieldsType = Map[String, ValuePack[Any]]

  val clazzName = getClass.getName
  val clazzKey = if (clazzName.endsWith("$")) clazzName.substring(0, clazzName.length - 1) else clazzName

  StructRegistry.register(clazzKey, this)

  def structName = clazzKey

  def topDoc = false


  override def typeInfo: TypeInfo =
    new TypeInfo(structName, t_memo_,
      for (fieldName <- orderedKeys.reverse) yield {
        val dt = fieldMap(fieldName)
        FieldDesc(dt.t_code_, dt.typeInfo, requiredField.contains(dt), fieldDefault.contains(fieldName))
      })

  //todo:
  def parse(str: String): Pack = makeValuePackWithDefault(Map[String, ValuePack[Any]]())

  //def enableVersion = false

  protected var fieldMap = Map[String, DataType[Any]]()
  protected var fieldFuncMap = Map[String, () => DataType[Any]]()
  protected var requiredField = Set[DataType[Any]]()
  protected var fieldDefault = Map[String, () => Any]()

  private var orderedKeys = List[String]()

  case class FieldInfo(name: String, dt: DataType[Any], field: () => DataType[Any], require: Boolean, default: Option[() => Any])

  def getFieldFunc(name: String) = fieldFuncMap.get(name)

  private implicit class NestPathHelp(subs: Option[StructPath]) {
    def ::(field: StructField) = {
      subs match {
        case None => None
        case Some(p) => Some(field :: p)
      }
    }
  }

  def getPathByCode(codePath: String): Option[StructPath] = {
    val vp = if (codePath.startsWith("/")) codePath.substring(1) else codePath
    if (vp.isEmpty) None
    else
      getPathByCode(vp.split("/").toList)
  }

  def getPathByCode(codeList: List[String]): Option[StructPath] = {

    codeList match {
      case this.t_code_ :: tail => //路径开头包含了自己
        getSubPathByCode(tail) match {
          case Some(innerPath) => Some((() => this) :: innerPath)
          case None => None
        }
      case _ => getSubPathByCode(codeList) match { //路径开头不包含了自己
        case Some(innerPath) => Some((() => this) :: innerPath)
        case None => None
      }
    }
  }

//  private def getSubPathByCode(codePath: String): Option[StructPath] = {
//    val vp = if (codePath.startsWith("/")) codePath.substring(1) else codePath
//    if (vp.isEmpty) None
//    else
//      getSubPathByCode(vp.split("/").toList)
//  }

  //传入参数是依次级联的 code , 返回 NestedStructFields 的 Option
  private def getSubPathByCode(codeList: List[String]): Option[StructPath] = {

    implicit class StringIsInt(str:String) {
      def isInt = {
        try {
          Integer.parseInt(str)
          true
        }catch {
          case e:Throwable => false
        }

      }
    }
    codeList match {
      case Nil =>
        None
      case code :: Nil =>
        if (fieldFuncMap.contains(code))
          Some(fieldFuncMap(code) :: Nil)
        else
          None
      case code :: tail =>
        if (fieldFuncMap.contains(code)) {
          val field = fieldFuncMap(code)
          field() match {  //三种情况(正常字段，TMap类型的一个Key,TList类型的一个序号）
            case dt: TStruct =>
              field :: dt.getSubPathByCode(tail)
            case dt : TList[_] if tail.size == 1 && tail.head.isInt =>
              Some(ListPath(List(field),tail.head.toInt))
            case dt: TList[_] =>
                throw new StructPathException(s"'${codeList.mkString("/")} is not a valid ListPath of data type '${dt.t_code_}'")
            case dt: TMap[_,_] if tail.size == 1 =>
              Some(MapPath(List(field),key = tail.head))
            case dt:TMap[_,_] =>
              throw new StructPathException(s"'${codeList.mkString("/")} is not a valid MapPath of data type '${dt.t_code_}'")
            case _ => None
          }
        }
        else
          None
    }
  }

  def fieldsInfo = for (name <- fieldMap.keys)
  yield FieldInfo(name, fieldMap(name), fieldFuncMap(name),
      requiredField.contains(fieldMap(name)), fieldDefault.get(name))

  def hasField(code: String) = fieldMap.contains(code)

  def hasField(dt: DataType[Any]) = fieldMap.contains(dt.t_code_)


  // <editor-fold defaultstate="collapsed" desc="构造 Struct 实例 ">

  def apply(values: ValuePack[Any]*): Pack = values.length match {
    case 1 =>
      values.apply(0) match {
        case p: Pack => apply(p.value)
        case _ => makeValuePackWithDefault((for (v <- values if hasField(v.code)) yield (v.code, v)).toMap)
      }
    case _ => makeValuePackWithDefault((for (v <- values if hasField(v.code)) yield (v.code, v)).toMap)
  }

  def apply(v: StructValue): Pack = makeValuePackWithDefault(v.values)

  def unapply(p: Pack) = if (p == null) None else Some(p)

  def fieldCount = fieldMap.size

  protected def makeValuePack(fieldValues: fieldsType) =
    makeValuePackWithDefault(fieldValues)

  //创建对象时检查有缺省值的字段，如果没有设置值，则设置缺省值
  private def makeValuePackWithDefault(fieldValues: fieldsType): Pack = {
    val defaults = for {
      (code, default) <- fieldDefault if !fieldValues.contains(code)
      fieldPack = fieldMap.get(code) match {
        case Some(dt: DataType[Any]) =>
          dt.AnyToPack(default())
        case _ => None
      }
      if fieldPack.isDefined
    } yield code -> fieldPack.get

    if (defaults.size > 0)
      Pack(StructValue(fieldValues ++ defaults, fieldMap, requiredField), 0)
    else
      Pack(StructValue(fieldValues, fieldMap, requiredField), 0)
  }

  // </editor-fold>

  override def AnyToPack(v: Any): Option[Pack] = v match {
    case mv: Pack => Some(mv)
    case _ => None
  }

  def packHashCode(p: Pack, superCode: Int) = superCode


  //此类型和下面的隐式转换构成 字段赋值的 “:= 语法”
  implicit class FieldTrans(val field: () => this.type) {
    def :=(value: StructValue) = field().apply(value)
  }

  //todo:
  protected def itemToString[VT](i: ValuePack[VT]): String = i.value.toString

  protected final def Field[T <: DataType[Any]](dt: T, isRequired: Boolean = false): () => T = {
    if (hasField(dt.t_code_)) throw new DuplicateFieldTypeException(s"${this.t_code_} has  duplicate field code: ${dt.t_code_} ")
    if (isRequired) requiredField = requiredField + dt
    fieldMap = fieldMap + (dt.t_code_ -> dt)
    fieldFuncMap = fieldFuncMap + (dt.t_code_ -> dtFun _)
    orderedKeys = dt.t_code_ :: orderedKeys
    def dtFun = {
      //      if (_debug) {
      //        println(s"set [${dt.t_code_}] parent as [$t_code_}]")
      //      }
      dt.prePathNode.set(TStruct.this)
      dt
    }
    dtFun _
  }

  // <editor-fold defaultstate="collapsed" desc="支持默认值的字段定义 ">

  implicit def funcToByName[T](func: () => T) = func()

  protected final def FieldStruct[T <: TStruct](dt: T, isRequired: Boolean = false, default: => T#Pack = null): () => T = {
    val dtFun = Field(dt, isRequired)
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    dtFun
  }

  protected final def FieldList[T <: TList[_]](dt: T, isRequired: Boolean = false, default: => T#Pack = null): () => T = {
    val dtFun = Field(dt, isRequired)
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    dtFun
  }

  protected final def FieldList[T <: TList[_]](dt: T) : () => T = {
    FieldList(dt,isRequired = true,dt(Nil))
  }

  protected final def FieldMap[T <: TMap[_, _]](dt: T, isRequired: Boolean = false, default: => T#Pack = null): () => T = {
    val dtFun = Field(dt, isRequired)
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    dtFun
  }

  protected final def FieldMap[T <: TMap[_, _]](dt: T): () => T = {
    FieldMap(dt,isRequired = true,default = dt(Map()))
  }


  protected final def FieldString[T <: TString](dt: T, isRequired: Boolean = false, default: => String = null): () => T = {
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    Field(dt, isRequired)
  }

  protected final def FieldByte[T <: TByte](dt: T, isRequired: Boolean = false, default: => Byte): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldShort[T <: TShort](dt: T, isRequired: Boolean = false, default: => Short): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldInt[T <: TInt](dt: T, isRequired: Boolean = false, default: => Int): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldLong[T <: TLong](dt: T, isRequired: Boolean = false, default: => Long): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldFloat[T <: TFloat](dt: T, isRequired: Boolean = false, default: => Float): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldDouble[T <: TDouble](dt: T, isRequired: Boolean = false, default: => Double): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  protected final def FieldBigDecimal[T <: TBigDecimal](dt: T, isRequired: Boolean = false, default: => BigDecimal = null): () => T = {
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    Field(dt, isRequired)
  }

  protected final def FieldBoolean[T <: TBoolean](dt: T, isRequired: Boolean = false, default: => Boolean): () => T = {
    fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    Field(dt, isRequired)
  }

  import com.github.nscala_time.time.Imports._

  protected final def FieldDateTime[T <: TDateTime](dt: T, isRequired: Boolean = false, default: => DateTime = null): () => T = {
    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> (() => default))
    }
    Field(dt, isRequired)
  }

  protected final def FieldEnum[T <: TEnum](dt: T, isRequired: Boolean = false, default: => Enumeration#Value = null): () => T = {

    val transDefault: () => EnumIdx = () => EnumIdx(default.id)

    if (default != null) {
      fieldDefault = fieldDefault + (dt.t_code_ -> transDefault)
    }
    Field(dt, isRequired)
  }

  // </editor-fold>


  implicit def fieldFunc2FieldType[T <: DataType[Any]](func: () => T) = func()

  implicit def CompoundPack(i: Pack): StructValue = i.value

  implicit def value2Pack(v: StructValue) = makeValuePackWithDefault(v.values)

  def validators: List[Pack => Option[String]] = List(RequireFieldValidator)

  def fromJsValue(x: JsValue) = Format.read(x)

  implicit object Format extends JsonFormat[Pack] {

    import spray.json._

    def read(json: JsValue): Pack = json match {
      case x: JsObject =>
        val fieldValues = x.fields.map {
          case (key, jsv) if fieldMap.contains(key) => key -> fieldMap(key).fromJsValue(jsv)
        }.toMap

        makeValuePack(fieldValues)

      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }

    def write(pack: Pack): JsValue = pack.toJsValue
  }

  protected def afterPackCreated(p: Pack) = ()

  object RequireFieldValidator extends (Pack => Option[String]) {
    override def apply(p: Pack): Option[String] = {
      val missing = for (dt <- requiredField if !p.values.contains(dt.t_code_)) yield dt.t_code_

      if (missing.size > 0) {
        val msg = missing.mkString(";")
        Some(s"fields [$msg] is required.")
      }
      else None
    }
  }

  case class Pack(value: StructValue, revise: Int)
    extends {
      val dataType = TStruct.this
    } with StructPack[StructValue]  with Serializable{

    //----------------------------序列化 begin
    private def writeObject(os: ObjectOutputStream): Unit = writeObjectDo(os)

    private def readObject(is: ObjectInputStream) = readObjectDo(is)

    private def readResolve: AnyRef = readResolveDo

    //----------------------------序列化 end

    def apply(packs: ValuePack[Any]*) = plusList(packs)

    def isOnlyRequireFields: Boolean = value.isOnlyRequireFields

    def fieldCount: Int = value.size

    override def withType[VT2](dt: DataType[VT2]): Option[ValuePack[VT2]] =
      if (dt == dataType) {
        //        if (_debug)
        //          println(s"in $t_code_ , output this StructPack ")
        Some(this.asInstanceOf[ValuePack[VT2]])
      }
      else {
        //        if (_debug)
        //          println(s"in $t_code_ , try output this StructPack field ${dt.t_code_} ")
        value.apply(dt)
      }

    //获取部嵌套的结构类型数据
    def subStruct[T <: TStruct](dt: T): Option[T#Pack] = {
      value.apply(dt) match {
        case Some(p) => Some(p.asInstanceOf[T#Pack])
        case None => None
      }
    }

    override def withCode(code: String): Option[ValuePack[Any]] = {
      val standCode = code.substring(0, 1).toLowerCase + code.substring(1, code.length)
      if (standCode == dataType.t_code_) Some(this) else value.apply(standCode)
    }

    //根据路径的字符串取出对应的值
    def valueByPath(pathCode:String) : Option[Any] = {
      dataType.getPathByCode(pathCode) match {
        case Some(path) =>
          path.getDomainData(this)
        case None => None
      }
    }

    def field[F <: DataType[Any]](ft: F): Option[F#Pack] = {
      val v = withType(ft)
      v match {
        case None => None
        case Some(p) =>
          Some(p.asInstanceOf[F#Pack])
      }
    }

    override def plus[VT2](i: ValuePack[VT2]): Pack = Pack(value plus i, revise + 1)

    override def plusList(vs: Seq[ValuePack[Any]]): Pack = Pack(value plusList vs, revise + 1)

    def -(dt: DataType[Any]): Pack = Pack(value sub dt, revise + 1)

    def -(vts: Seq[DataType[Any]]): Pack = Pack(value subList vts, revise + 1)

    def <~(fieldBlock: => ValuePack[Any]): Pack = try {
      Pack(value plus fieldBlock, revise + 1)
    } catch {
      case ex: Throwable => this
    }

    def <~~(fieldsBlock: => Seq[ValuePack[Any]]): Pack = try {
      Pack(value plusList fieldsBlock, revise + 1)
    } catch {
      case ex: Throwable => this
    }

    //相同的字段去掉(除非是必填项),一方有一方没有的字段留下，不同的字段保留左边的
    def xor(other: Pack): Pack = {
      Pack(value xor other.value, revise + 1)
    }


    override def toString = itemToString(this)

    def validate: List[String] = validate(validators)

    def version: Int = this(VersionInfo) match {
      case None => 0
      case Some(v) => v(Ver).get.value
    }

    //对指定的某个字段的值执行一个函数，用函数的结果值替换这个字段的值，返回新的结构实例
    def update[VT](dt: TMono[VT], f: VT => VT): Pack = {

      this(dt) match {
        case None => this
        case Some(pack) => this <~ dt(f(pack.value))
      }
    }

    override def toJsValue: JsValue = JsObject {
      value.values.map {
        case (key, vp) => key -> vp.toJsValue
      }
    }

    afterPackCreated(this)

    override def hashCode(): Int = {
      packHashCode(this, super.hashCode())
    }

    override def ->>(other: ValuePack[Any]): Pack = {
      if (other.dataType != dataType) throw new RuntimeException("data type miss match")
      val o = other.asInstanceOf[Pack]
      Pack(this.value ->> o.value, revise + 1)
    }
  }

  object Pack extends DefaultJsonProtocol {
    implicit val packJsonFormat = new RootJsonFormat[Pack] {
      override def read(json: JsValue): Pack = fromJsValue(json)

      override def write(obj: Pack): JsValue = obj.toJsValue
    }
  }

}

object TStruct extends TStruct {
  override val t_memo_ : String = "StructType"

  //异种数据结构之间的数据拷贝
  def copy[F <: TStruct#Pack, T <: TStruct#Pack](from: F, to: T, items: Seq[CopyField], merge: MergeManner.Value = MergeManner.UseNew): T = {

    var newDoc = to
    items.foreach { i =>
      val newData = from(i.from.inStructPath.through) match {
        case None => None
        case Some(v: TMono[_]#Pack) =>
          i.to.inStructPath.through.AnyToPack(v)
        case Some(v) => Some(v)
      }
      newDoc = i.to.alterDomainData(newDoc, newData, merge)
    }
    newDoc
  }

  case class CopyField(from: NodePath, to: NodePath)

}

object StructRegistry {
  //保存所有的 TStruct 实例,以 typeName 为key
  private[this] val _registry = new AtomicReference(Map.empty[String, TStruct])

  @tailrec
  def register(key: String, dt: TStruct): TStruct = {
    val reg = _registry.get
    val updated = reg.updated(key, dt)
    if (_registry.compareAndSet(reg, updated)) dt
    else register(key, dt)
  }

  def get(key: String): Option[TStruct] = _registry.get.get(key)

  def all = _registry.get.map(_._2).toList.sortWith((s1, s2) => s1.typeInfo.valueCode.compareTo(s2.typeInfo.valueCode) < 0)

  def tops = all.filter(_.topDoc)
}


class DuplicateFieldTypeException(val msg: String) extends Exception(msg)

class RequiredFieldNotSetException(val msg: String) extends Exception(msg)

class StructPathException(val msg: String) extends Exception(msg)


/**
 * 为 TStruct#Pack 提供 java Bean 封装的辅助特质
 * @tparam T TStruct 类型
 */
trait PackBean[T <: TStruct] {
  var p: T#Pack

  def property[VT](dt: TMono[VT]): VT = {
    p(dt) match {
      case Some(v) => v.value
      case None => null.asInstanceOf[VT]
    }
  }

  def property[ST <: TStruct, SBT <: PackBean[ST]](dt: ST, maker: ST#Pack => SBT): SBT = {
    p.subStruct(dt) match {
      case Some(v) => maker(v)
      case None => null.asInstanceOf[SBT]
    }
  }
}
