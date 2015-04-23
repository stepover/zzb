package zzb.rest.authentication

/**
 * Created by Simon on 2014/7/12
 */
import com.typesafe.config.{ Config, ConfigException }
import spray.util.pimpString_

import scala.concurrent.Future

object UserPassAuthenticator {

  def apply[T](f: UserPassAuthenticator[T]) = f

  /**
   * Creats a UserPassAuthenticator that uses plain-text username/password definitions from a given
   * spray/akka config file section for authentication. The config section should look like this:
   * {{{
   *   zzb.rest.users {
   *     username = "password"
   *     ...
   *   }
   * }}}
   */
  def fromConfig[T](config: Config)(createUser: UserPass ⇒ T): UserPassAuthenticator[T] =
    userPassOption ⇒
      Future.successful {
        userPassOption.flatMap { userPass ⇒
          try {
            val pw = config.getString(userPass.user)
            if (pw secure_== userPass.pass) Some(createUser(userPass)) else None
          } catch {
            case _: ConfigException ⇒ None
          }
        }
      }
}

//object CachedUserPassAuthenticator {
//  /**
//   * Creates a wrapper around an UserPassAuthenticator providing authentication lookup caching using the given cache.
//   * Note that you need to manually add a dependency to the spray-caching module in order to be able to use this method.
//   */
//  def apply[T](inner: UserPassAuthenticator[T], cache: Cache[Option[T]] = LruCache[Option[T]]())(implicit ec: ExecutionContext): UserPassAuthenticator[T] =
//    userPassOption ⇒
//      cache(userPassOption) {
//        inner(userPassOption)
//      }
//}
