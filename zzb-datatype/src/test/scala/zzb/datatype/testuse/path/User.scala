package zzb.datatype.testuse.path

import zzb.datatype._
import zzb.datatype.testuse._

object BloodType extends Enumeration with TEnum {
  val t_memo_ = "血型"
  val A = Value(1, "A")
  val B = Value(2, "B")
  val O = Value(3, "O")
  val AB = Value(4, "AB")
}

object Car extends TStruct {
  val license = Field(TString("license", "车牌号"))
  val vin = Field(TString("vin", "车架号"))
  val t_memo_ : String = "车辆信息"
}

object User extends TStruct {

  val name = Field(TString("name", "姓名"), isRequired = true)
  val age = Field(TInt("age", "年龄"))
  val height = Field(TFloat("height", "身高"))
  val blood = FieldEnum(BloodType, default = () => BloodType.A)
  val birthDay = Field(TDateTime("birthday", "初生日期"))
  val male = Field(TBoolean("male", "性别"))

  import BasicFormats._
  val Phones = TList[Int]("phones","电话号码")
  val phones = Field(Phones)

  val misc = FieldMap(TProperty("misc","杂项"),default = TProperty.empty)


  val car = Field(Car)

  val t_memo_ : String = "用户信息"

  implicit def packWrap(p: Pack): PackWrap = new PackWrap(p)

  class PackWrap(p: Pack) {
    def name = p(User.name).get.value
  }
}

object Student extends TStruct {

  val sname = Field(TString("sname", "姓名"))
  val sage = Field(TInt("sage", "年龄"))
  val height = Field(TFloat("height", "身高"))
  val blood = FieldEnum(BloodType, default = () => BloodType.A)
  val birthDay = Field(TDateTime("birthday", "初生日期"))
  val sex = Field(TBoolean("sex", "性别"))

  val car = Field(Car)

  val t_memo_ : String = "学生信息"

  implicit def packWrap(p: Pack) = new PackWrap(p)

  class PackWrap(p: Pack) {
    def name = p(User.name).get.value
  }

}