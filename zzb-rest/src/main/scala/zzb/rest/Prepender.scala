package zzb.rest

import shapeless._

trait Prepender[P <: HList, S <: HList] {
  type Out <: HList
  def apply(prefix: P, suffix: S): Out
}

object Prepender {
  implicit def hnilPrepend[P <: HList, S <: HNil] = new Prepender[P, S] {
    type Out = P
    def apply(prefix: P, suffix: S) = prefix
  }

  implicit def apply[P <: HList, S <: HList, Out0 <: HList](implicit prepend: PrependAux[P, S, Out0]) =
    new Prepender[P, S] {
      type Out = Out0
      def apply(prefix: P, suffix: S): Out = prepend(prefix, suffix)
    }
}