package zzb.service

import com.typesafe.config.Config
import akka.actor.ActorSystem

import scala.collection.JavaConversions._
import zzb.db.{Mongos, DBPools}
import zzb.srvbox.SrvManageProtocol.ServiceReady

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-12
 * Time: 上午10:47
 * Copyright baoxian.com 2012~2020
 */
abstract class BoxedService(val system: ActorSystem, val config: Config) {

  var sharedSystem = false
  var dbNames: List[String] = Nil
  var mongoDBNames: List[String] = Nil

  val serviceName = config.getString("box.service-name")

  val starter = system.actorSelection(s"/user/boxActor/$serviceName")

  final def serviceReady() = starter ! ServiceReady(serviceName)

  def startup(serviceName: String, sharedSystem: Boolean): Unit = {

    this.sharedSystem = sharedSystem

    if (config.hasPath("db"))
      initDB(config.getConfig("db"))
    if (config.hasPath("mongo"))
      initMongoDB(config.getConfig("mongo"))

    init()
  }

  def init() = {
    serviceReady()
  }
  def fini(): Unit = {}

  private def initDB(dbconfig: Config) = {
    dbNames = dbconfig.getStringList("usedb").toList
    dbNames.map(name ⇒ DBPools.openDB(name, dbconfig.getConfig(name)))
  }

  private def initMongoDB(dbconfig: Config) = {
    mongoDBNames = dbconfig.getStringList("usedb").toList
    mongoDBNames.map(name ⇒ Mongos.openDB(name, dbconfig.getConfig(name)))
  }
  /**
   * Callback run on  shutdown.
   * Shutdown actor systems here.
   */
  def shutdown(): Unit = {
    fini()
    dbNames.map(DBPools.closeDB)
    mongoDBNames.map(Mongos.closeDB)
    if (!sharedSystem)
      system.shutdown()
  }

}
