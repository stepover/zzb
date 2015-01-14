package zzb.datatype
import scala.language.implicitConversions

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-25
 * Time: 下午1:12
 * Copyright baoxian.com 2012~2020
 */

trait TFixKeysMap[K, V] extends TMap[K, V] {

  val fixKeys: Set[K]

  override val itemFilter :  ItemFilter = kv => fixKeys.contains(kv._1)

  implicit def packWrap(p:Pack): TFixKeysMap.this.type#PackWrap = new PackWrap(p)

  class PackWrap(p:Pack){
     def availableKeys = fixKeys
     def allowKeys(keyFilter: K => Boolean) = fixKeys.filter(keyFilter)
  }
}
