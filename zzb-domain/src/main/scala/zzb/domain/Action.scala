package zzb.domain

import spray.json.DefaultJsonProtocol
import zzb.datatype.{DataType, ValuePack}
import zzb.domain.directive.AuthorizedOperator

import scala.collection.mutable

/**
 * Created by Simon on 2014/7/18
 */
class Action(val name: String, val opt: AuthorizedOperator, val params: Map[String, String], val entity: Option[ValuePack[_]]) extends AllowDelay

trait AllowDelay {
  override def toString = getClass.getSimpleName.replace("$", "")
}

object Action {

  def apply(name: String, opt: AuthorizedOperator, params: Map[String, String], entity: Option[ValuePack[_]]) =
    new Action(name, opt, params, entity)

  def unapply(a: Action) = if (a == null)
    None
  else
    Some(a.name, a.opt, a.params, a.entity)
}

case class ActionDefine(name: String, roleCheck: AuthorizedOperator => Boolean, entityType: Option[DataType[_]])

/**
 * 所有通过Rest API 提交的动作结果都是这个类型
 * @param param 当前文档状态的索引值
 * @param msg 附带的消息
 * @param ver 当前文档的版本信息
 */
case class ActionResult(param: Int, msg: String, ver: VersionRevise)

object ActionResult extends DefaultJsonProtocol {
  implicit val format = jsonFormat3(ActionResult.apply)
}

object A1{
  def unapply(a: Action) = if (a == null)
    None
  else
    Some(a.name)
}

object A2{
  def unapply(a: Action) = if (a == null)
    None
  else
    Some(a.name,a.opt)
}

object A3{
  def unapply(a: Action) = if (a == null)
    None
  else
    Some(a.name,a.opt,a.params)
}

object A4{
  def unapply(a: Action) = if (a == null)
    None
  else
    Some(a.name,a.opt,a.params, a.entity)
}

trait ActionBuilder {

  val actions = mutable.Map[String, ActionDefine]()

  def action(name: String, roleCheck: AuthorizedOperator => Boolean, dt: DataType[_] = null) = {
    actions(name) = new ActionDefine(name, roleCheck, Option(dt))
  }

  def roleAny: AuthorizedOperator => Boolean = r => true

  def roleUser: AuthorizedOperator => Boolean = r => if (!r.isManager) true else false

  def roleManager: AuthorizedOperator => Boolean = r => if (r.isManager) true else false

  def roles(rs: Set[String]): AuthorizedOperator => Boolean = r => if (r.roles.keys.exists(rs.contains)) true else false

}