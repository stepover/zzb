package zzb.rest

import directives._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-1
 * Time: 上午10:34
 * Copyright baoxian.com 2012~2020
 */
trait Directives extends RouteConcatenation
  with AnyParamDirectives
  with BasicDirectives
  with ExecutionDirectives
  with ForwardDirectives
  with FormFieldDirectives
  with FutureDirectives
  with HeaderDirectives
  with MethodDirectives
  with MiscDirectives
  with MarshallingDirectives
  with ParameterDirectives
  with PathDirectives
  with RouteDirectives
  with RespondWithDirectives
  with ZzbDatatypeDirectives
  with SecurityDirectives

object Directives extends Directives