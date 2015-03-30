package zzb.datatype.demo

import zzb.datatype._
import scala.collection.immutable.Iterable
import spray.json._
import scala.reflect._

trait Suite extends TStruct {

  implicit def ensureItemPack2Define(pack: Item.Pack) = new ItemPackWrap(pack)

  type ItemValidator = Item.Pack => Option[String]

  case class ItemSelector(template: ItemDef, selectIdx: Int = 0,
                          amount: BigDecimal = -1, min: BigDecimal = 0, max: BigDecimal = 0, variable: Boolean = true)

  object ItemDefType extends TMono[ItemDef] {
    def parse(str: String): ItemDefType.Pack = ???

    val vtm = classTag[ItemDef]

    val t_memo_ : String = "保障项目定义"
    override lazy val t_code_ : String = "itemDefine"
    implicit val valueFormat: JsonFormat[ItemDef] = ItemDefJsonFormat
  }

  implicit object ItemDefJsonFormat extends JsonFormat[ItemDef] {
    def write(x: ItemDef) = JsString(x.code)

    def read(value: JsValue) = value match {
      case JsString(x) => availableItems.find(_.code == x).get
      case x => deserializationError("Expected String as JsString, but got " + x)
    }
  }

  object Item extends TStruct {
    val t_memo_ : String = "保障项目"

    val itemDef = Field(ItemDefType, isRequired = true)
    val insureAmount = Field(TBigDecimal("insureAmount", "保险金额"))
    val charge = Field(TBigDecimal("charge", "实际保费"))
    val listPrice = Field(TBigDecimal("listPrice", "原价"))
    val discountRate = Field(TFloat("discountRate", "折扣率"))
  }

  class ItemPackWrap(val pack: Item.Pack) {

    def itemDefine: ItemDef = pack(Item.itemDef()).get.value

    def toSelector: ItemSelector = itemDefine.item2Selector(pack)

    def doValidate(): Seq[String] = doValidate(itemDefine.itemValidators)

    def doValidate(validator: ItemValidator) = validator(pack)

    def doValidate(validators: Seq[ItemValidator]): Seq[String] =
      for {
        validator <- validators
        res = validator(pack)
        if res.isDefined
      } yield res.get
  }

  /** 保障项目定义 */
  trait ItemDef {

    /** 保障项代码 */
    val code: String
    /** 保障项名称 */
    val name: String
    /** 可选项 */
    val options: Map[Int, String] = Map(0 -> "不投保", 1 -> "投保")

    val optionsAmount: Map[Int, BigDecimal] = Map()

    val allowFreeFill: Boolean = false

    /** 校验规则 */
    def itemValidators = Seq[ItemValidator]()

    /** 保障项描述 */
    val desc: String = name


    override def hashCode() = code.hashCode

    def Selector(idx: Int = 0) =
      if (!options.contains(idx)) throw new Exception(s"no option for idx = $idx")
      else ItemSelector(this, idx)

    def makeItem(idx: Int) = selector2Item(Selector(idx))

    def item2Selector(item: Item.Pack, variable: Boolean = true): ItemSelector = {
      import Item._

      val (idx, amount) = item(insureAmount()) match {
        case None => (1, BigDecimal(-1)) // 只要Item在suite 中存在，就不是 0(不投保）,-1 表示未知的保额
        case Some(d) =>
          val findIdx = for ((idx, amount) <- optionsAmount if d.value == amount) yield idx
          val idx = if (findIdx.size > 0) findIdx.head else 1
          (idx, d.value)
      }

      ItemSelector(this, idx, amount, amount, amount, variable)
    }

    def selector2Item(sel: ItemSelector): Item.Pack = {
      import Item._

      val amount: BigDecimal = optionsAmount.getOrElse(sel.selectIdx, -1)

      Item.apply(itemDef := ItemDef.this, insureAmount := amount)
    }
  }

  /** ****************    模板属性 Begin  ***************************************/

  /** 所有保障项目的模板 */
  def availableItems: Set[ItemDef]

  /** 必须投保的保障项目 */
  def requiredItems = Set[ItemDef]()

  /** 每一个保障项目对应一个其他保障项目的列表，该列表是其投保的前置条件 */
  def preConditions = Map[ItemDef, List[ItemDef]]()

  /** 对保障项目的校验规则集合 */
  def itemsValidators = List[EnsuiteValidator](RequiredValidator(requiredItems), PreConditionValidator(preConditions), ForAllItemsValidator)

  /** ****************    模板属性 End    ***************************************/

  /** ****************    内部字段 Begin  ***************************************/
  val charge = Field(TBigDecimal("charge", "总保费"))

  val discountRate = Field(TDouble("discountRate", "折扣率"))

  val startDate = Field(TDateTime("startDate", "生效日期"))

  val items = Field(InnerItems, isRequired = true)

  /** ****************    内部字段 End    ***************************************/

  //private val me = this

  private def suiteCode = t_code_


  type FilterType = ((ItemDef, Item.Pack)) => Boolean

  implicit def pack2wrap(p: Pack) = new PackWrap(p)

  /** 创建一个空的 Suite 对象，所有字段未设置，保障项目为空，允许所有的保障项目 */
  def apply(): Pack = apply(InnerItems(Map[ItemDef, Item.Pack]()))

  /** 创建一个空的 Suite 对象，所有字段未设置，保障项目为空，filter 控制允许的保障项目允许的保障项目 */
  def apply(filter: FilterType): Pack = apply(InnerItems(Map[ItemDef, Item.Pack](), filter))


  /**
   * 根据所给的字段值创建一个 Suite 对象，如果没有给保障项目，则（保障项目为空，允许所有的保障项目），
   * 如果给出了保障项目，则过滤规则按照给出的保障项目已有的定义
   */
  override def apply(values: AnyRef*) = {
    val items = values.filter{
      case vp :ValuePack[_] if vp.value != null => true
      case Some(vp:ValuePack[_]) if vp.value != null => true
      case _ => false
    }.map{
      case vp :ValuePack[_]  => vp
      case Some(vp:ValuePack[_])  => vp
    }
    if (items.exists(v => v.dataType == InnerItems))
      innerApply(items)
    else
      innerApply(InnerItems(Map[ItemDef, Item.Pack]()) :: items.toList)
  }


  /**
   * 根据说给的字段值和保障项目过滤器创建一个 Suite 对象，如果没有给保障项目，则保障项目为空
   * 如果给出了保障项目，用过滤器过滤给出的保障项目
   */
  def makeWithFilter(filter: FilterType, values: AnyRef*): Pack = {
    val items = values.filter{
      case vp :ValuePack[_] if vp.value != null => true
      case Some(vp:ValuePack[_]) if vp.value != null => true
      case _ => false
    }.map{
      case vp :ValuePack[_]  => vp
      case Some(vp:ValuePack[_])  => vp
    }
    if (items.exists(v => v.dataType == InnerItems))
      innerApply(filter, innerApply(items))
    else
      innerApply(InnerItems(Map[ItemDef, Item.Pack](), filter) :: items.toList)
  }

  private def innerApply(filter: FilterType, v: Pack): Pack = {
    val fieldItems = InnerItems(v.Items.value, filter)
    v <~ fieldItems
  }

  private def innerApply(values: Seq[ValuePack[Any]]) =
    makeValuePack((for (v <- values if hasField(v.code)) yield (v.code, v)).toMap)


  object InnerItems extends TFixKeysMap[ItemDef, Item.Pack] {
    val suiteTemplate = me
    val fixKeys = availableItems
    val t_memo_ : String = "保险项目集合"
    override lazy val t_code_ = suiteCode + "." + getClass.getSimpleName.replace("$", "")

    //override def validators = itemsValidators
    implicit val keyFormat: JsonFormat[ItemDef] = ItemDefJsonFormat
    implicit val valueFormat: JsonFormat[Item.Pack] = Item.Format
    override val vm = classTag[ItemDef]
    override val km = classTag[Item.Pack]
  }

  type EnsuiteValidator = Pack => Option[String]

  /** 检查投保项的依赖条件 */
  case class PreConditionValidator(conditions: Map[ItemDef, List[ItemDef]]) extends EnsuiteValidator {
    def apply(suite: Pack) = {
      val items = suite.Items

      var messages = List[String]()

      for ((define: ItemDef, _) <- items) {
        if (conditions.contains(define))
          for (depDefine <- conditions(define) if !items.contains(depDefine))
            messages = s"投保 '${define.name}' 要求 '${depDefine.name}' 必须也投保" :: messages

      }
      if (messages.size > 0) Some(messages.mkString("\n")) else None
    }

    //todo: 定义序列化方式
  }

  /** 检查必须投保的项目是否投保 */
  case class RequiredValidator(requiredItems: Set[ItemDef]) extends EnsuiteValidator {
    def apply(suite: Pack) = {
      val items = suite.Items

      val messages: Iterable[String] = for {
        requiredItem <- requiredItems
        if !items.contains(requiredItem)
      } yield s" '${requiredItem.name}' 必须投保'"

      if (messages.size > 0) Some(messages.mkString("\n")) else None
    }

    //todo: 定义序列化方式
  }

  /** 检查每个选择的投保的项目是否合法 */
  object ForAllItemsValidator extends EnsuiteValidator {
    def apply(suite: Pack) = {
      val items = suite.Items

      var messages = List[String]()

      for ((define, item: Item.Pack) <- items) {
        val itemReports = item.doValidate()
        if (itemReports.length > 0)
          messages = itemReports.mkString(";") :: messages
      }

      if (messages.size > 0) Some(messages.mkString("\n")) else None
    }

    //todo: 定义序列化方式
  }

  /** 因为没有办法向 Pack 中增加方法，就实现这个封装器，在其中增加方法，然后通过隐式转换调用 */
  class PackWrap(val p: Pack) {

    def apply(itemDef: ItemDef): Option[Item.Pack] = Items.get(itemDef)

    /** 向  suite 中增加一个新的保障项目,如果该项目已经存在，会替换。返回一个新的 Suite.Pack */
    def ~(item: Item.Pack): Pack = p <~ Items.plusWith(item.itemDefine, item)


    /** 向  suite 中增加一组新的保障项目,如果某个项目已经存在，会替换。返回一个新的 Suite.Pack */
    def ~(xs: Seq[Item.Pack]): Pack = p <~ Items.plusWithList(xs.map(x => (x.itemDefine, x)))

    /** 获取 suite 中的保障项目 */
    def Items = p.value(items()).get.asInstanceOf[InnerItems.Pack]

    def doValidate(): Seq[String] = doValidate(itemsValidators)

    def doValidate(validator: EnsuiteValidator) = validator(p)

    def doValidate(validators: Seq[EnsuiteValidator]): Seq[String] =
      for {
        validator <- validators
        res = validator(p)
        if res.isDefined
      } yield res.get

    //获取指定项目的保费
    def charge(itemDef: ItemDef): Option[BigDecimal] =
      Items.get(itemDef) match {
        case None => None
        case Some(item) =>
          item(Item.charge()) match {
            case None => None
            case Some(v) => Some(v.value)
          }
      }


    def foreach[U](f: ((ItemDef, Item.Pack)) => U): Unit = Items.foreach[U](f)

    def map(f: ((ItemDef, Item.Pack)) => (ItemDef, Item.Pack)): Pack = p <~ Items.map(f)

    def filter(f: ((ItemDef, Item.Pack)) => Boolean): Pack = p <~ Items.filter(f)

    def withFilter(f: ((ItemDef, Item.Pack)) => Boolean) = p <~ Items.withFilter(f)
  }

}
