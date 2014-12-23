package zzb.domain

import spray.http.LazyValueBytesRenderable

/**
 * Created by Simon on 2014/6/11
 */
trait ActionError  extends LazyValueBytesRenderable {
  def intValue: Int

  def value: String = intValue.toString + ' ' + reason

  def reason: String

  def isSuccess: Boolean = false

  def isFailure: Boolean = true
}

