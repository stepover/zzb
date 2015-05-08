package zzb.storage.data

import com.github.nscala_time.time.Imports._
import spray.json.JsonFormat
import zzb.datatype._
import zzb.storage.TStorable

import scala.reflect._

object ID extends TString {
  val t_memo_ = "身份证"
}

object UserName extends TString {
  val t_memo_ = "用户名"
}

trait Age extends TInt {
  val t_memo_ = "用户年龄"
}

object BloodType extends Enumeration with TEnum {

  val t_memo_ = "血型"

  val A = Value(1, "A")
  val B = Value(2, "B")
  val O = Value(3, "O")
  val AB = Value(4, "AB")
}

object UserHeight extends TFloat {
  val t_memo_ = "用户身高"
}

object UserAge extends Age

object DriverAge extends Age

object UserInfo extends TStruct {

  val userName = Field(UserName, isRequired = true)
  val userAge = Field(UserAge)
  val driverAge = Field(DriverAge)
  val userHeight = Field(UserHeight)
  val blood = FieldEnum(BloodType, default = () => BloodType.A)

  val birthDay = Field(TDateTime("birthday", "初生日期"))

  val male = Field(TBoolean("male", "性别"))

  val track = Field(TrackInfos)

  val t_memo_ : String = "用户信息"

}

/**
 * 任务轨迹信息
 */
object TrackInfos extends TrackInfoListBase {
  implicit val elementFormat: JsonFormat[TrackInfo.Pack] = TrackInfo.Format

  override def itemDataType: DataType[Any] = TrackInfo
}

/**
 * 轨迹信息列表基础类
 */
trait TrackInfoListBase extends TPackList[TrackInfo.Pack] {
  val t_memo_ = "轨迹信息列表"
  override val lm: ClassTag[_] = classTag[TrackInfo.Pack]
}

object TrackInfo extends TrackInfoBase


/**
 * 轨迹信息
 */
trait TrackInfoBase extends TStruct {
  val t_memo_ : String = "轨迹信息"

  val actionName = Field(TString("actionName", "动作名称"))

  val operator = Field(TString("operator", "操作人"))

  val remark = Field(TString("remark", "备注"))

  val operaTime = FieldDateTime(TDateTime("operaTime", "操作时间"), default = () => DateTime.now)
}


object CarLicense extends TString {

  val t_memo_ = "车牌号"
}

object CarVin extends TString {

  val t_memo_ = "车架号"
}

object CarInfo extends TStruct {

  val carLicense = Field(CarLicense)
  val carVin = Field(CarVin)
  val createDate = FieldDateTime(TDateTime("created", "创建时间"), isRequired = true, default = () => DateTime.now)
  val t_memo_ : String = "车辆信息"
}

object HomeInfo extends TStorable[String, ID.type] {

  //这两行定义主键字段
  override val keyType = ID

  val userId = Field(ID, isRequired = true)


  val userInfo = Field(UserInfo)
  val carInfo = Field(CarInfo)
  val t_memo_ : String = "家庭信息"

  import spray.json.DefaultJsonProtocol._

  val intList = Field(TList[Int]("intList", "intList") /*,default = List(5,8,6)*/)

  val trackMap = FieldMap(TrackMap)

  val stopTimes = FieldMap(TStrKeyMap[Int]("stopTimes", "经停时间"))

  val vips = FieldMap(TStrKeyMap[Boolean]("vips", "记录乘客是否为vip"))


}


object HomeInfoError extends TStorable[String, ID.type] {

  override val keyType = ID
  val id = Field(ID) //错误，主键没有设置为必填字段

  val userInfo = Field(UserInfo)
  val carInfo = Field(CarInfo)
  val t_memo_ : String = "家庭信息"

}

object TrackMap extends TStrKeyPackMap[TrackInfo.Pack]{
  override def valueDataType: DataType[Any] = TrackInfo

  override val km: ClassTag[_] = classTag[String]
  override val vm: ClassTag[_] = classTag[TrackInfo.Pack]
  override implicit val keyFormat: JsonFormat[String] = zzb.datatype.BasicFormats.StringJsonFormat
  override implicit val valueFormat: JsonFormat[TrackInfo.Pack] = TrackInfo.Format
  override val t_memo_ : String = "TrackMap"
}