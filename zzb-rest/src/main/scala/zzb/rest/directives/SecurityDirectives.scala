package zzb.rest
package directives

import scala.concurrent.{ ExecutionContext, Future }
import shapeless.HNil
import zzb.rest.authentication._
import BasicDirectives._
import FutureDirectives._
import MiscDirectives._
import RouteDirectives._

/**
 * Created by Simon on 2014/7/12
 */
trait SecurityDirectives {

  /**
   * Wraps its inner Route with authentication support.
   * Can be called either with a ``Future[Authentication[T]]`` or ``ContextAuthenticator[T]``.
   */
  def authenticate[T](magnet: AuthMagnet[T]): Directive1[T] = magnet.directive

  /**
   * Applies the given authorization check to the request.
   * If the check fails the route is rejected with an [[zzb.rest.AuthorizationFailedRejection]].
   */
  def authorize(check: ⇒ Boolean): Directive0 = authorize(_ ⇒ check)

  /**
   * Applies the given authorization check to the request.
   * If the check fails the route is rejected with an [[zzb.rest.AuthorizationFailedRejection]].
   */
  def authorize(check: RestReqContext ⇒ Boolean): Directive0 =
    extract(check).flatMap[HNil](if (_) pass else reject(AuthorizationFailedRejection)) &
      cancelRejection(AuthorizationFailedRejection)
}

class AuthMagnet[T](authDirective: Directive1[Authentication[T]])(implicit executor: ExecutionContext) {
  val directive: Directive1[T] = authDirective.flatMap {
    case Right(user)     ⇒ provide(user)
    case Left(rejection) ⇒ reject(rejection)
  }
}

object AuthMagnet {
  implicit def fromFutureAuth[T](auth: ⇒ Future[Authentication[T]])(implicit executor: ExecutionContext): AuthMagnet[T] =
    new AuthMagnet(onSuccess(auth))

  implicit def fromContextAuthenticator[T](auth: ContextAuthenticator[T])(implicit executor: ExecutionContext): AuthMagnet[T] =
    new AuthMagnet(extract(auth).flatMap(onSuccess(_)))
}
