package zzb.rest

import scala.concurrent.Future

/**
 * Created by Simon on 2014/7/12
 */
package object authentication {
  //# auth-types
  type Authentication[T] = Either[Rejection, T]
  type ContextAuthenticator[T] = RestReqContext ⇒ Future[Authentication[T]]
  //#
  //# user-pass-authenticator
  type UserPassAuthenticator[T] = Option[UserPass] ⇒ Future[Option[T]]
  //#
}

package authentication {

  /**
   * Simple case class model of a username/password combination.
   */
  case class UserPass(user: String, pass: String)

  /**
   * A very basic user context object.
   * In your application you probably want to use some more specific custom class.
   */
  case class BasicUserContext(username: String)

}