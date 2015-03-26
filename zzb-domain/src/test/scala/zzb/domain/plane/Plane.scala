package zzb.domain.plane

import zzb.datatype._
import scala.reflect._
import spray.json._
import spray.json.DefaultJsonProtocol._
import zzb.storage.TStorable

/**
 * Created by Simon on 2014/5/14
 */
object Plane extends TStorable[String, TString] {

  override def topDoc = true

  implicit val passengerFormat = Passenger.Format

  val Passengers = TPackList(Passenger, "passengers", "乘客列表")

  implicit val cargoFormat = Cargo.Format

  val AirStops = TList[String]("airStops", "经停机场")

  val DistanceStops = TList[Int]("distanceStops", "每站距离")

  val Cargos = TStrKeyPackMap(Cargo, "cargos", "货物")

  val StopTimes = TStrKeyMap[Int]("stopTimes","经停时间")

  val Vips = TStrKeyMap[Boolean]("vips","记录乘客是否为vip")

  //-------------------------------

  val id = FieldString(TString("id", "编号"), isRequired = true)

  val owner = FieldString(TString("owner", "所有者"), isRequired = true)

  val height = FieldInt(TInt("height", "高度"), isRequired = true, default =  0)

  val speed = FieldInt(TInt("speed", "速度"), isRequired = true, default =  0)

  val foods = Field(Foods)

  val passengers = Field(Passengers)

  val airStops = FieldList(AirStops)

  val distanceStops = FieldList(DistanceStops)

  val memo = Field(TString("memo", "备注"))

  val cargo = FieldMap(Cargos)

  val stopTimes = FieldMap(StopTimes)

  val vips = FieldMap(Vips)

  //用于单元测试检查结果
  val message = FieldString(TString("message", "信息"), isRequired = true,default = "")

  val state = FieldEnum(PlaneState, isRequired = true, () => PlaneState.Stopped)

  implicit class PackWrap(p: Pack) {
    def id = p(Plane.id()).get.value

    def state = {
      val s = p(Plane.state)
      PlaneState(s.get.value.idx)
    }

    def owner = p(Plane.owner()).get.value
  }

  override val t_memo_ : String = "飞机"
  override val keyType: TString = id()
}

trait Passenger extends TStruct {
  val name = FieldString(TString("name", "姓名"), isRequired = true)
  val age = Field(TInt("age", "年龄"))
}

object Passenger extends Passenger {
  override val t_memo_ : String = "乘客"
}

object PlaneState extends Enumeration with TEnum {
  val t_memo_ : String = "飞行状态"
  override lazy val t_code_ : String = "state"

  val Executing = Value(0, "执行中")

  val Stopped = Value(1, "停止")

  val Sliding = Value(2, "滑行中")

  val Flying = Value(3, "飞行中")

}

trait Cargo extends TStruct {
  val id = Field(TString("id", "编号"), isRequired = true)
  val owner = Field(TString("owner", "所有人"))
}

object Cargo extends Cargo {
  override val t_memo_ : String = "货物"
}

object Foods extends TStruct {
  override val t_memo_ : String = "机上食物"

  val water = Field(TInt("water", "水"))
  val bread = Field(TInt("bread", "面包"))
}