//package zzb.rest.mvm
//
//import akka.testkit.ImplicitSender
//import akka.remote.testkit.{MultiNodeSpec, MultiNodeConfig}
//import akka.actor.{ActorIdentity, Identify}
//
//
///**
//* Created by Simon on 2014/5/26
//*/
//object DataEntityMultiNodeSpec extends MultiNodeConfig {
//  commonConfig(debugConfig(on = false))
//
//  val master = role("master")
//  val slave = role("slave")
//
//  testTransport(on = true)
//}
//
//class DataEntityNodeSpec extends MultiNodeSpec(DataEntityMultiNodeSpec) with STMultiNodeSpec with ImplicitSender {
//  override def initialParticipants: Int = 2
//
//  import DataEntityMultiNodeSpec._
//
//  lazy val echo = {
//    system.actorSelection(node(master) / "user" / "echo") ! Identify(None)
//    expectMsgType[ActorIdentity].ref.get
//  }
//
//  "sss" must  {
//    "ddd" taggedAs LongRunningTest in  {
//
//    }
//  }
//}
//
//
//class DataEntityMultiNode1 extends DataEntityNodeSpec{
//
//}
//
//class DataEntityMultiNode2 extends DataEntityNodeSpec{
//
//}
//
//
//
