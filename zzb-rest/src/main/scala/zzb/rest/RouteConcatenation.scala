package zzb.rest

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-1
 * Time: 上午10:36
 * Copyright baoxian.com 2012~2020
 */
trait RouteConcatenation {

  implicit def pimpRouteWithConcatenation(route: Route) = new RouteConcatenation(route: Route)

  class RouteConcatenation(route: Route) {

    /**
     * Returns a Route that chains two Routes. If the first Route rejects the request the second route is given a
     * chance to act upon the request.
     */
    def ~(other: Route): Route = { ctx ⇒
      route {
        ctx.withRejectionHandling { rejections ⇒
          other(ctx.withRejectionsMapped(rejections1 ⇒ rejections ++ rejections1))
        }
      }
    }
  }

}

object RouteConcatenation extends RouteConcatenation