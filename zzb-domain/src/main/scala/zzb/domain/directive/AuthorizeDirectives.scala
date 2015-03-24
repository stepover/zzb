package zzb.domain.directive

import zzb.rest._

import scala.concurrent.{ExecutionContext, Future}
import shapeless.HNil
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import spray.json.DefaultJsonProtocol
import spray.util._

/**
 * Created by Simon on 2014/3/25
 */
trait AuthorizeDirectives {

  import directives.HeaderDirectives._
  import directives.BasicDirectives._
  import directives.RouteDirectives._

  /**
   * 提取当前请求的认证操作者
   * @return
   */
//  def operator: Directive1[AuthorizedOperator] = headerValue(optionalOperatorValue("x-user-id", isManager = false)) |
//    headerValue(optionalOperatorValue("x-manager-id", isManager = true)) |
//    reject(AuthorizationFailedRejection)

  def operator: Directive1[AuthorizedOperator] = headersValue(takeOperator,"x-???-id") |
    reject(AuthorizationFailedRejection)

  private def takeOperator(headers: List[RestHeader]):Option[AuthorizedOperator] = {
    val roles = headers.filter(header => header.lowercaseName.startsWith("x-")
      && header.lowercaseName.endsWith("-id")).map(header => header.lowercaseName.substring(2) -> header.value).map(
    kv => kv._1.substring(0,kv._1.length - 3) -> kv._2)
    val managers = Set("manager","admin","system")
    if(roles.size == 0) Some(AuthorizedOperator.UnknownOperator)
    else{
      val rolesMap = roles.toMap
      val isManager = rolesMap.keys.exists(managers.contains)
      Some(AuthorizedOperator(roles(0)._2,isManager,rolesMap))
    }
  }

  /**
   * Extracts an HTTP header value using the given function. If the function result is undefined for all headers the
   * request is rejected with an empty rejection set. If the given function throws an exception the request is rejected
   * with a [[zzb.rest.MalformedHeaderRejection]].
   */
  def headersValue[T](f: List[RestHeader] ⇒ Option[T],headInfo:String): Directive1[T] = {
    val protectedF: List[RestHeader] ⇒ Option[Either[Rejection, T]] = headers ⇒
      try f(headers).map(Right.apply)
      catch {
        case NonFatal(e) ⇒ Some(Left(MalformedHeaderRejection(headInfo, e.getMessage.nullAsEmpty, Some(e))))
      }
    extract(ctx => protectedF(ctx.request.headers)).flatMap  {
      case Some(Right(a))        ⇒ provide(a)
      case Some(Left(rejection)) ⇒ reject(rejection)
      case None                  ⇒ reject
    }
  }

  def operatorCheck(f:Map[String,String] => Boolean):Directive0 = operator.require(opt => f(opt.roles),AuthorizationFailedRejection)
  def operatorIs(role:String): Directive0 = operator.require(_.roles.keySet.contains(role),AuthorizationFailedRejection)
  def operatorIs(roles:Set[String]): Directive0 = operator.require(_.roles.keys.exists(roles.contains),AuthorizationFailedRejection)

  def operatorNot(role:String): Directive0 = operator.require(!_.roles.keySet.contains(role),AuthorizationFailedRejection)
  def operatorNot(roles:Set[String]): Directive0 = operator.require(_.roles.keys.forall(r => !roles.contains(r)),AuthorizationFailedRejection)

  private def optionalValue(lowerCaseName: String): RestHeader ⇒ Option[String] = {
    case RestHeader(`lowerCaseName`, value) ⇒ Some(value)
    case _ ⇒ None
  }

  private def optionalOperatorValue(lowerCaseName: String, isManager: Boolean): RestHeader ⇒ Option[AuthorizedOperator] = {
    case RestHeader(`lowerCaseName`, value) ⇒
      Some(AuthorizedOperator(value, isManager))
    case _ ⇒ None
  }

  def authorize(owner: => Future[Option[String]])(implicit ec: ExecutionContext) = operator.flatMap {
    case opt => OwnerFutureMagnet.apply(owner,opt)
  }

  trait OwnerFutureMagnet extends Directive0

  object OwnerFutureMagnet {

    implicit def apply(owner: => Future[Option[String]],opt:AuthorizedOperator)(implicit ec: ExecutionContext) =
      new OwnerFutureMagnet {
        override def happly(f: (HNil) => Route): Route = ctx ⇒
          try {
            owner.onComplete {
              case Success(Some(optName)) if opt.isManager || opt.id == optName ⇒ f(HNil)(ctx)
              case Success(_) ⇒ ctx.reject(AuthorizationFailedRejection)
              case Failure(error) ⇒ ctx.failWith(error)
            }
          }
          catch {
            case NonFatal(error) ⇒ ctx.failWith(error)
          }
      }
  }

}

case class AuthorizedOperator(id: String, isManager: Boolean,roles:Map[String,String]=Map()){
  override def toString = s"${if (isManager) "m"else "u"}:$id"
}
object AuthorizedOperator extends DefaultJsonProtocol{
  implicit val format = jsonFormat3(AuthorizedOperator.apply)

  val UnknownOperator = AuthorizedOperator("_unknown_",false)
}
