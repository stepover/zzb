package zzb.rest

import com.github.nscala_time.time.Imports._
import spray.json.JsonFormat

import scala.reflect._

/**
 * Created by Simon on 2014/6/24
 */
package object testuse {

  import zzb.datatype._
  import BasicFormats._

  val Phones = TList[Int]("phones", "电话号码")

  object Student extends TStruct {

    val name = Field(TString("name", "姓名"))
    val age = Field(TInt("age", "年龄"))
    val height = Field(TFloat("height", "身高"))
    val birthDay = Field(TDateTime("birthday", "初生日期"))
    val sex = FieldBoolean(TBoolean("sex", "性别"), isRequired = true, default = true)
    val phones = Field(Phones)

    val t_memo_ : String = "学生信息"

    implicit def packWrap(p: Pack) = new PackWrap(p)

    class PackWrap(p: Pack) {
      def name = p(Student.name).get.value
    }

  }

  object Task extends TStruct {
    val t_memo_ = "在某状态下可执行的动作信息列表"
    val createDate = FieldString(TString("createDate", "任务创建日期"), default = () ⇒ DateTime.now.toString("yyyy-MM-dd"))
    val isAssignToUser = FieldBoolean(TBoolean("isAssignToUser", "是否分配到人"), isRequired = true, default = true)
    val track = Field(TrackInfos)
  }

  object TrackInfo extends TrackInfoBase

  trait TrackInfoListBase extends TPackList[TrackInfo.Pack] {
    val t_memo_ = "轨迹信息列表"
    override val lm: ClassTag[_] = classTag[TrackInfo.Pack]
  }

  /**
   * 任务轨迹信息
   */
  object TrackInfos extends TrackInfoListBase {
    implicit val elementFormat: JsonFormat[TrackInfo.Pack] = TrackInfo.Format

    override def itemDataType: DataType[Any] = TrackInfo
  }

  /**
   * 轨迹信息
   */
  trait TrackInfoBase extends TStruct {
    val t_memo_ : String = "轨迹信息"

    val actionName = Field(TString("actionName", "动作名称"))

    val operator = Field(TString("operator", "操作人"))

    val operaTime = FieldDateTime(TDateTime("operaTime", "操作时间"), default = () ⇒ DateTime.now)
  }

}
