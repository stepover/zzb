package zzb.datatype

import scala.reflect._
import spray.json.JsonFormat

/**
 * Created by Simon on 2014/3/28
 */

trait Versioned { this : TStruct =>

  /** 内置的 版本信息 字段 */
  val verInfo = FieldStruct(VersionInfo, isRequired = true,() => VersionInfo())
}

object VersionInfo extends TStruct{
  val t_memo_ : String = "VersionInfo"
  override lazy val t_code_ = "verInfo"


  val ver = FieldInt(Ver, isRequired = true, default = () => 0)
  val time = Field(SaveTime)
  val opt = Field(Operator)
  val isOwn = Field(IsOwnerOperate)
  //val del = FieldBoolean(Del, isRequired = true, default = () => false)
}
object VersionInfos extends TPackList[VersionInfo.Pack]{
  override val t_memo_ : String = "VersionList"
  override implicit val elementFormat: JsonFormat[VersionInfo.Pack] = VersionInfo.Format
  override val lm = classTag[VersionInfo.Pack]

  override def itemDataType: DataType[Any] = VersionInfo
}


object Ver extends TInt {
  val t_memo_ : String = "version"
  override lazy val t_code_ = "ver"
}

object Del extends TBoolean {
  val t_memo_ : String = "deleted"
  override lazy val t_code_ = "del"
}

object SaveTime extends TDateTime{
  val t_memo_ : String = "saveTime"
  override lazy val t_code_ = "time"
}

object Operator extends TString{
  val t_memo_ : String = "operator"
  override lazy val t_code_ = "opt"
}

object IsOwnerOperate extends TBoolean{
  val t_memo_ : String = "owner"
  override lazy val t_code_ = "isOwn"
}


