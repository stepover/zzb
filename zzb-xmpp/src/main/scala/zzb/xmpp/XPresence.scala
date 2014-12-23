package zzb.xmpp

/**
 * Created by Simon on 2014/8/8
 */
trait XPresence{
  override def toString = this.getClass.getSimpleName.replace("$","")
}

object Online extends XPresence

object Offline extends XPresence

object Connecting extends XPresence

