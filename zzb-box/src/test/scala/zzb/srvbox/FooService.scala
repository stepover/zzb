package zzb.srvbox

import zzb.service.BoxedService
import com.typesafe.config.Config
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-13
 * Time: 下午4:27
 * Copyright baoxian.com 2012~2020
 */
class S1Service(system: ActorSystem, config: Config) extends BoxedService(system, config) {

  override def init() {super.init()}
}

class S2Service(system: ActorSystem, config: Config) extends BoxedService(system, config) {

  override def init() {super.init()}

}

class S3Service(system: ActorSystem, config: Config) extends BoxedService(system, config) {

}

class S4Service(system: ActorSystem, config: Config) extends BoxedService(system, config) {

}

class S5Service(system: ActorSystem, config: Config) extends BoxedService(system, config) {

  override def init() {super.init()}

}

