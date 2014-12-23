package zzb.domain

/**
 * Created by Simon on 2014/7/18
 */
sealed trait Operator

case object Manager extends Operator

case object System extends Operator

case object User extends Operator