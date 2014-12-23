package zzb.rest
package directives

import org.specs2.mutable.Specification
import zzb.rest.testkit.Specs2RouteTest

class RoutingSpec extends Specification with Directives with Specs2RouteTest {

  val Ok = RestResponse()
  val completeOk = complete(Ok)

  def echoComplete[T]: T ⇒ Route = { x ⇒ complete(x.toString) }
  def echoComplete2[T, U]: (T, U) ⇒ Route = { (x, y) ⇒ complete(s"$x $y") }
}