package zzb.datatype

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import zzb.datatype.demo._


class EnsureTest extends WordSpec with MustMatchers {

  "Ensure define" must {
    "can create new empty suite" in {
      import BizSuite._
      //创建一个空的suite
      val suite = BizSuite()
      val items = suite.Items

      val canUseItemCount = items.availableKeys.size
      items.usedKeys.size must equal(0)
      suite.fieldCount must equal(2) //还有一个缺省的version_

    }

    "can create suite with some field" in {

      import BizSuite._

      val suite = BizSuite(charge := 1000.1, discountRate := 0.9)
      suite.fieldCount must equal(3+1)
      suite(charge).get.toString must equal("1000.1")
    }

    "can add some items to suite" in {
      import BizSuite._

      val suite0 = BizSuite(charge := 1000.1, discountRate := 0.9)

      val suite1 = suite0 ~ VehicleDemageIns.makeItem(1)

      suite1.Items.usedKeys.size must equal(1)

      val suite2 = suite1 ~ List(NcfVehicleDemageIns.makeItem(1), ThirdPartyIns.makeItem(1))
      suite2.Items.usedKeys.size must equal(3)

      val suite3 = BizSuite(charge := 1000.1, discountRate := 0.9, suite2.Items)
      suite3.Items.usedKeys.size must equal(3)
    }

    "can create empty suite with filter " in {
      import BizSuite._
      val excludeItem = Set(NcfTheftIns, ThirdPartyIns, DriverIns)
      val keyFilter = (itemDef: ItemDef) => !excludeItem.contains(itemDef)
      val filter = (kv: (ItemDef, Item.Pack)) => !excludeItem.contains(kv._1)

      val suite0 = BizSuite(filter)

      val items = suite0.Items

      val availableCount = items.availableKeys.size
      val allowCount = items.allowKeys(keyFilter).size

      (availableCount - allowCount) must equal(excludeItem.size)

      items.usedKeys.size must equal(0)
      suite0.fieldCount must equal(1+1)

    }
    "can create not empty suite with filter" in {
      import BizSuite._

      val excludeItem = Set(NcfTheftIns, ThirdPartyIns, DriverIns)
      val keyFilter = (itemDef: ItemDef) => !excludeItem.contains(itemDef)
      val filter = (kv: (ItemDef, Item.Pack)) => !excludeItem.contains(kv._1)

      val suite0 = BizSuite(filter, charge := 1000.1, discountRate := 0.9)
      val items = suite0.Items

      val availableCount = items.availableKeys.size
      val allowCount = items.allowKeys(keyFilter).size

      (availableCount - allowCount) must equal(excludeItem.size)

      items.usedKeys.size must equal(0)
      suite0.fieldCount must equal(3+1)

      val suite1 = suite0 ~ NcfTheftIns.makeItem(1) //被排除的保障项目应该加不进去
      suite1.Items.usedKeys.size must equal(0)

      val suite2 = suite1 ~ VehicleDemageIns.makeItem(1)
      suite2.Items.usedKeys.size must equal(1)

      val suite3 = suite2 ~ NcfTheftIns.makeItem(1) //suite 复制时过滤器会被保存
      suite3.Items.usedKeys.size must equal(1)


      val suite4 = suite3 ~ List(NcfTheftIns.makeItem(1), NcfVehicleDemageIns.makeItem(1)) //suite 复制时过滤器会被保存
      suite4.Items.usedKeys.size must equal(2)

      val suite5 = BizSuite(filter, charge := 1000.1, discountRate := 0.9, suite4.Items)
      suite5.Items.usedKeys.size must equal(2)
    }

    "can get selector from Item" in {

      import BizSuite._

      val suite0 = BizSuite(charge := 1000.1, discountRate := 0.9) ~ ThirdPartyIns.makeItem(2)

      val items = suite0.Items

      val item: Item.Pack = items.get(ThirdPartyIns).get

      val selector = item.toSelector

      selector.selectIdx must equal(2)
    }

    "can do validate " in {
      {
        import BizSuite._

        val suite0 = BizSuite(charge := 1000.1, discountRate := 0.9)

        val messages0 = suite0.doValidate
        messages0.size must equal(1)

        val suite1 = suite0 ~ List(NcfVehicleDemageIns.makeItem(1), DriverIns.makeItem(1))

        val messages1 = suite1.doValidate
        messages1.size must equal(3)

        val suite2 = suite1 ~ DriverIns.makeItem(1)(Item.charge := 99) //保费小于100，仍然会报错

        val messages2 = suite2.doValidate
        messages2.size must equal(3)

        val suite3 = suite2 ~ DriverIns.makeItem(1)(Item.charge := 100) //错误已修正

        val messages3 = suite3.doValidate
        messages3.size must equal(2)
      }
      {
        import ForceSuite._
        val s0 = ForceSuite()
        s0.doValidate.size must equal(0)
      }
    }
    "can do map filter foreach " in {
      import BizSuite._

      val suite0 = BizSuite(charge := 1000.1, discountRate := 0.9) ~ List(VehicleDemageIns.makeItem(1),
        NcfVehicleDemageIns.makeItem(1), ThirdPartyIns.makeItem(1))

      suite0.Items.usedKeys.size must equal(3)
      val suite1 = suite0.filter { kv => true }

      suite1.Items.usedKeys.size must equal(3)

      suite0.Items.usedKeys.size must equal(3)
      val suite2 = suite0.filter { kv => kv._1 != VehicleDemageIns  }
      suite2.Items.usedKeys.size must equal(2)

      //所有保障项目的保费都设置为500
      val suite3: BizSuite.Pack = for( kv <- suite0 ) yield {
        (kv._1,kv._2 <~ Item.charge()(500))
      }

//      val suite3: BizSuite.Pack = (suite0).map {
//        case (define, item) => {
//          (define,item <~ EnsureItem.charge()(500))
//        }
//      }

      suite3.charge(VehicleDemageIns).get must equal(BigDecimal(500))

      val suite4 = for( (define,item) <- suite3 ) yield {
        if (define == VehicleDemageIns)
          (define,item <~ Item.charge()(800))
        else (define,item)
      }

      suite4.charge(VehicleDemageIns).get must equal(BigDecimal(800))
      suite4.charge(ThirdPartyIns).get must equal(BigDecimal(500))

      val suite5 = for( (define,item) <- suite4 if define == VehicleDemageIns) yield {
          (define,item <~ Item.charge()(900))
      }

      suite5.charge(VehicleDemageIns).get must equal(BigDecimal(900))
      suite5.charge(ThirdPartyIns) must be (None)

      var defines = List[ItemDef]()

      for((define,item) <- suite3) defines = define :: defines
      defines.size must equal(3)

    }
  }


}
