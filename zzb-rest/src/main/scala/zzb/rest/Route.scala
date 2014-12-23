package zzb.rest

import shapeless.HList

object Route {
  def apply(f: Route): Route = f

  /**
   * Converts the route into a directive that never passes the request to its inner route
   * (and always returns its underlying route).
   */
  def toDirective[L <: HList](route: Route): Directive[L] = new Directive[L] {
    def happly(f: L ⇒ Route) = route
  }
}