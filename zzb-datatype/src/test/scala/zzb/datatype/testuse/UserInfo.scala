package zzb.datatype.testuse

import spray.json.JsonFormat
import zzb.datatype._

import scala.reflect._

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

object UserHeight extends TInt {
  val t_memo_ = "用户身高"
}

object NoThis extends TMono[Null] {
  val vtm =  classTag[Null]
  val t_memo_ = "头发数量"
  def parse(str: String) = ???
  implicit val valueFormat: JsonFormat[Null] = null
}

object UserAge extends Age
object DriverAge extends Age

/**
 * Created by Simon on 2014/7/2
 */
object UserInfo extends TStruct with Versioned {

  val userName = Field(UserName,isRequired = true)
  val userAge = Field(UserAge)
  val driverAge = Field(DriverAge)
  val userHeight = Field(UserHeight)
  val blood = FieldEnum(BloodType,default = BloodType.A)
  val birthDay = Field(TDateTime("birthday","初生日期"))
  val male = Field(TBoolean("male","性别"))
  val memo = Field(TString("memo","备注"))
  val t_memo_ : String = "用户信息"

  val phones = Field(Phones)

  val misc = FieldMap(TProperty("misc","杂项"),default = TProperty.empty)

  implicit class PackWrap(p: Pack) {
    def userName = {
      val idt = UserInfo.userName()
      val op = p(idt)
      val op2 = p.withCode(idt.t_code_)
      op.get.value
    }
    def misc = p(UserInfo.misc()).get.asInstanceOf[TProperty.Pack]
  }

}

object Student extends TStruct{
  val t_memo_ : String = "学生信息"
  val userName = Field(UserName)
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
  val t_memo_ : String = "车辆信息"
}

object HomeInfo extends TStruct {
  val userInfo = Field(UserInfo)
  val carInfo = Field(CarInfo)
  val t_memo_ : String = "家庭信息"
  val friends = FieldList(Users,isRequired = true,default =  Users(List[UserInfo.Pack]()))
}
