package zzb.domain.directive

import zzb.rest._
import scala.concurrent.{ExecutionContext, Future}
import shapeless.HNil
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import spray.json.DefaultJsonProtocol

/**
 * Created by Simon on 2014/3/25
 */
trait AuthorizeDirectives {

  import directives.HeaderDirectives._
  import directives.RouteDirectives._

  /**
   * 提取当前请求的认证操作者
   * @return
   */
  def operator: Directive1[AuthorizedOperator] = headerValue(optionalOperatorValue("x-user-id", isManager = false)) |
    headerValue(optionalOperatorValue("x-manager-id", isManager = true)) |
    reject(AuthorizationFailedRejection)

  def operatorIs(opt:AuthorizedOperator): Directive0 = operator.require(_.equals(opt),AuthorizationFailedRejection)

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

case class AuthorizedOperator(id: String, isManager: Boolean){
  override def toString = s"${if (isManager) "m"else "u"}:$id"
}
object AuthorizedOperator extends DefaultJsonProtocol{
  implicit val format = jsonFormat2(AuthorizedOperator.apply)
}
