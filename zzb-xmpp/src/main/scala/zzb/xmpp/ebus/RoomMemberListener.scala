package zzb.xmpp.ebus

import org.jivesoftware.smackx.muc.ParticipantStatusListener

/**
 * Created by Simon on 2014/8/12
 */
abstract class RoomMemberListener extends ParticipantStatusListener{
  override def kicked(participant: String, actor: String, reason: String): Unit = ()

  override def voiceGranted(participant: String): Unit = ()

  override def voiceRevoked(participant: String): Unit = ()

  override def banned(participant: String, actor: String, reason: String): Unit = ()

  override def membershipGranted(participant: String): Unit = ()

  override def membershipRevoked(participant: String): Unit = ()

  override def moderatorGranted(participant: String): Unit = ()

  override def moderatorRevoked(participant: String): Unit = ()

  override def ownershipGranted(participant: String): Unit = ()

  override def ownershipRevoked(participant: String): Unit = ()

  override def adminGranted(participant: String): Unit = ()

  override def adminRevoked(participant: String): Unit = ()

  override def nicknameChanged(participant: String, newNickname: String): Unit = ()
}
