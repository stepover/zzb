package zzb.service

import spray.routing._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-17
 * Time: 下午2:40
 * Copyright baoxian.com 2012~2020
 */
trait RestSupport extends Directives {

  def RestRequest(ctx: RequestContext) = routes(ctx)

  def routes: Route
}
