package zzb.storage

import zzb.datatype.{Versioned, DataType, TStruct}

/**
 * Created by Simon on 2014/3/31
 */
trait TStorable[K,KT <: DataType[K] ] extends TStruct with Versioned{

  def keyType: KT

  override protected def afterPackCreated(p:Pack) =  require(requiredField.contains(keyType))
}
