package zzb.rest
package authentication

import com.typesafe.config.Config
import scala.concurrent.{ ExecutionContext, Future }
import spray.http._
import spray.util._
import HttpHeaders._
import AuthenticationFailedRejection._

/**
 * Created by Simon on 2014/7/12
 */
trait HttpAuthenticator[U] extends ContextAuthenticator[U] {

  def apply(ctx: RestReqContext) = {
    val authHeader = ctx.request.headers.findByType[`Authorization`]
    val credentials = authHeader.map { case Authorization(creds) ⇒ creds }
    authenticate(credentials, ctx) map {
      case Some(userContext) ⇒ Right(userContext)
      case None ⇒
        val cause = if (authHeader.isEmpty) CredentialsMissing else CredentialsRejected
        Left(AuthenticationFailedRejection(cause, getChallengeHeaders(ctx.request)))
    }
  }

  implicit def executionContext: ExecutionContext

  def authenticate(credentials: Option[HttpCredentials], ctx: RestReqContext): Future[Option[U]]

  def getChallengeHeaders(restRequest: RestRequest): List[RestHeader]
}

/**
 * The BasicHttpAuthenticator implements HTTP Basic Auth.
 */
class BasicHttpAuthenticator[U](val realm: String, val userPassAuthenticator: UserPassAuthenticator[U])(implicit val executionContext: ExecutionContext)
  extends HttpAuthenticator[U] {

  def authenticate(credentials: Option[HttpCredentials], ctx: RestReqContext) = {
    userPassAuthenticator {
      credentials.flatMap {
        case BasicHttpCredentials(user, pass) ⇒ Some(UserPass(user, pass))
        case _                                ⇒ None
      }
    }
  }

  def getChallengeHeaders(httpRequest: RestRequest) =
    `WWW-Authenticate`(HttpChallenge(scheme = "Basic", realm = realm, params = Map.empty)) :: Nil

}

object BasicAuth {
  def apply()(implicit settings: RestSettings, ec: ExecutionContext): BasicHttpAuthenticator[BasicUserContext] =
    apply("Secured Resource")

  def apply(realm: String)(implicit settings: RestSettings, ec: ExecutionContext): BasicHttpAuthenticator[BasicUserContext] =
    apply(realm, userPass ⇒ BasicUserContext(userPass.user))

  def apply[T](realm: String, createUser: UserPass ⇒ T)(implicit settings: RestSettings, ec: ExecutionContext): BasicHttpAuthenticator[T] =
    apply(realm, settings.users, createUser)

  def apply[T](realm: String, config: Config, createUser: UserPass ⇒ T)(implicit ec: ExecutionContext): BasicHttpAuthenticator[T] =
    apply(UserPassAuthenticator.fromConfig(config)(createUser), realm)

  def apply[T](authenticator: UserPassAuthenticator[T], realm: String)(implicit ec: ExecutionContext): BasicHttpAuthenticator[T] =
    new BasicHttpAuthenticator[T](realm, authenticator)
}