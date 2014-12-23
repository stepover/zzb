package zzb.datatype.demo

import zzb.datatype.Versioned


/** 车险商业险模板 */
object BizSuite extends Suite with Versioned{
  val t_memo_ : String = "车险商业险模板"

  // override lazy val t_code_ : String = "biz"


  object VehicleDemageIns extends ItemDef {
    val code: String = "VehicleDemageIns"
    val name: String = "车辆损失险"


  }

  object NcfVehicleDemageIns extends ItemDef {
    val code: String = "NcfVehicleDemageIns"
    val name: String = "附加车辆损失险不计免赔"
  }

  object ThirdPartyIns extends ItemDef {
    val code: String = "ThirdPartyIns"
    val name: String = "第三者责任险"
    /** 可选项 */
    override val options: Map[Int, String] = Map(
      0 -> "不投保",
      1 -> "5万", 2 -> "10万", 3 -> "15万", 4 -> "20万", 5 -> "30万",
      6 -> "50万", 7 -> "100万", 8 -> "150万", 9 -> "200万", 10 -> "250万", 11 -> "300万"
    )
    override val optionsAmount: Map[Int, BigDecimal] = Map(
      1 -> 50000, 2 -> 100000, 3 -> 150000, 4 -> 200000, 5 -> 300000,
      6 -> 500000, 7 -> 1000000, 8 -> 1500000, 9 -> 2000000, 10 -> 2500000, 11 -> 3000000
    )
  }

  object NcfThirdPartyIns extends ItemDef {
    val code: String = "NcfThirdPartyIns"
    val name: String = "附加第三者责任险不计免赔"
  }

  object DriverIns extends ItemDef {
    val code: String = "DriverIns"
    val name: String = "司机责任险"


    override def itemValidators = List(checker)

    private val checker: ItemValidator = (item: Item.Pack) => {
      item(Item.charge) match {
        case None => Some("车损险保费不能为空")
        case Some(d) if d.value.asInstanceOf[BigDecimal] < 100 => Some("车损险保费必须大于100")
        case _ => None
      }
    }

    /** 可选项 */
    override val options: Map[Int, String] = Map(
      0 -> "不投保",
      1 -> "5000", 2 -> "1万", 3 -> "2万", 4 -> "3万", 5 -> "4万",
      6 -> "5万", 7 -> "6万", 8 -> "7万", 9 -> "8万", 10 -> "9万", 11 -> "10万",
      12 -> "15万", 13 -> "20万"
    )
    override val optionsAmount: Map[Int, BigDecimal] = Map(
      1 -> 5000, 2 -> 10000, 3 -> 20000, 4 -> 30000, 5 -> 40000,
      6 -> 50000, 7 -> 60000, 8 -> 70000, 9 -> 80000, 10 -> 90000, 11 -> 100000,
      12 -> 150000, 13 -> 200000
    )
  }

  object NcfDriverIns extends ItemDef {
    val code: String = "NcfDriverIns"
    val name: String = "附加司机责任险不计免赔"
  }

  object PassengerIns extends ItemDef {
    val code: String = "PassengerIns"
    val name: String = "乘客责任险"
    /** 可选项 */
    override val options: Map[Int, String] = Map(
      0 -> "不投保",
      1 -> "5000", 2 -> "1万/座", 3 -> "2万/座", 4 -> "3万/座", 5 -> "4万/座",
      6 -> "5万/座", 7 -> "6万/座", 8 -> "7万/座", 9 -> "8万/座", 10 -> "9万/座", 11 -> "10万/座",
      12 -> "15万/座", 13 -> "20万/座"
    )
    override val optionsAmount: Map[Int, BigDecimal] = Map(
      1 -> 5000, 2 -> 10000, 3 -> 20000, 4 -> 30000, 5 -> 40000,
      6 -> 50000, 7 -> 60000, 8 -> 70000, 9 -> 80000, 10 -> 90000, 11 -> 100000,
      12 -> 150000, 13 -> 200000
    )
  }

  object NcfPassengerIns extends ItemDef {
    val code: String = "NcfPassengerIns"
    val name: String = "附加乘客责任险不计免赔"
  }

  object TheftIns extends ItemDef {
    val code: String = "TheftIns"
    val name: String = "全车盗抢险"
  }

  object NcfTheftIns extends ItemDef {
    val code: String = "NcfTheftIns"
    val name: String = "附加全车盗抢险不计免赔"
  }

  /** 商业险模板中可包含的所有保障项目的模板  */
  def availableItems = Set(
    VehicleDemageIns, NcfVehicleDemageIns, ThirdPartyIns, NcfThirdPartyIns, DriverIns, NcfDriverIns, PassengerIns,
    NcfPassengerIns, TheftIns, NcfTheftIns
  )

  /** 保障项目的依赖关系，一个保障项目可以依赖另外一组保障项目，这组保障项目都投保它才能投保  */
  override val preConditions: Map[ItemDef, List[ItemDef]]
  = Map(
    NcfVehicleDemageIns -> List(VehicleDemageIns),
    NcfThirdPartyIns -> List(ThirdPartyIns),
    NcfDriverIns -> List(DriverIns),
    NcfPassengerIns -> List(PassengerIns),
    NcfTheftIns -> List(TheftIns)
  )

  override def requiredItems = Set[ItemDef](VehicleDemageIns, ThirdPartyIns)
}

/** 车险交强险模板 */
object ForceSuite extends Suite {
  val t_memo_ : String = "车险交强险模板"
  override lazy val t_code_ : String = "force"
  val availableItems: Set[ItemDef] = Set(VehicleCompulsoryIns)

  object VehicleCompulsoryIns extends ItemDef {
    val code: String = "VehicleCompulsoryIns"
    val name: String = "交强险"
  }

}