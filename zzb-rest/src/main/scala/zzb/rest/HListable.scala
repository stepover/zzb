package zzb.rest

import shapeless._

/**
 * Provides a way to convert a value into an HList.
 * If the value is already an HList then it is returned unchanged, otherwise it's wrapped into a single-element HList.
 */
trait HListable[T] {
  type Out <: HList
  def apply(value: T): Out
}

object HListable extends LowerPriorityHListable {
  implicit def fromHList[T <: HList] = new HListable[T] {
    type Out = T
    def apply(value: T) = value
  }
}

private[rest] abstract class LowerPriorityHListable {
  implicit def fromAnyRef[T] = new HListable[T] {
    type Out = T :: HNil
    def apply(value: T) = value :: HNil
  }
}