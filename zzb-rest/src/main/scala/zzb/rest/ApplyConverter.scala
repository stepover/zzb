package zzb.rest

import shapeless._

abstract class ApplyConverter[L <: HList] {
  type In
  def apply(f: In): L ⇒ Route
}

object ApplyConverter extends ApplyConverterInstances {
  implicit val hac0 = new ApplyConverter[HNil] {
    type In = Route
    def apply(fn: In) = {
      case HNil ⇒ fn
    }
  }
}
