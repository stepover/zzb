package zzb.rest

import shapeless.HList

/**
 * A Route that can be implicitly converted into a Directive (fitting any signature).
 */
abstract class StandardRoute extends Route {
  def toDirective[L <: HList]: Directive[L] = StandardRoute.toDirective(this)
}

object StandardRoute {
  def apply(route: Route): StandardRoute = route match {
    case x: StandardRoute ⇒ x
    case x                ⇒ new StandardRoute { def apply(ctx: RestReqContext): Unit = { x(ctx) } }
  }

  /**
   * Converts the route into a directive that never passes the request to its inner route
   * (and always returns its underlying route).
   */
  implicit def toDirective[L <: HList](route: Route): Directive[L] = Route.toDirective(route)
}