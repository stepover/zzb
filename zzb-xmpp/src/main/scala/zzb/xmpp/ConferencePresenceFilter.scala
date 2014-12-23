package zzb.xmpp

import org.jivesoftware.smack.filter.PacketTypeFilter
import org.jivesoftware.smack.packet.{Packet, Presence}

/**
 * Created by Simon on 2014/8/12
 */


case class ConferencePresenceFilter(roomName:String) extends PacketTypeFilter(classOf[Presence]){
  override def accept(packet: Packet):Boolean = super.accept(packet) && packet.getFrom.startsWith(roomName)
}


object NotConferencePresenceFilter extends PacketTypeFilter(classOf[Presence]){
  override def accept(packet: Packet):Boolean = super.accept(packet) && !packet.getFrom.contains("conference")
}
