package zzb.db

import javax.sql.DataSource

import com.jolbox.bonecp.BoneCPDataSource
import com.typesafe.config.Config
import org.squeryl.adapters._
import org.squeryl.{ SessionFactory, Session }
import org.squeryl.internals.DatabaseAdapter
import com.typesafe.scalalogging.slf4j.Logging

import scala.collection.parallel.mutable
import scala.slick.driver.H2Driver

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-8-19
 * Time: 下午6:09
 * Copyright baoxian.com 2012~2020
 */
object DBPools extends Logging {

  private val driver = "driver"
  private val url = "url"
  private val user = "user"
  private val password = "password"
  private val pool_min = "pool.min"
  private val pool_max = "pool.max"
  private val pool_inc = "pool.inc"
  private val pool_partitionCount = "pool.partitionCount"
  //没有用
  private val pool_testTable = "pool.testTable"

  private val pool_idleMaxAge = "pool.idleMaxAge"
  private val pool_idleConnectionTestPeriod = "pool.idleConnectionTestPeriod"
  private val pool_statementsCacheSize = "pool.statementsCacheSize"

  private val pools: scala.collection.mutable.Map[String, BoneCPDataSource] = scala.collection.mutable.Map.empty
  private val poolAdapterCreators: scala.collection.mutable.Map[String, (() ⇒ DatabaseAdapter)] = scala.collection.mutable.Map.empty
  private val sessionFactories: scala.collection.mutable.Map[String, SessionFactory] = scala.collection.mutable.Map.empty

  private val adapterCreator: Map[String, () ⇒ DatabaseAdapter] = Map(
    "com.mysql.jdbc.Driver" -> (() ⇒ new MySQLAdapter),
    "org.h2.Driver" -> (() ⇒ new H2Adapter),
    "oracle.jdbc.OracleDriver" -> (() ⇒ new OracleAdapter),
    "org.postgresql.Driver" -> (() ⇒ new PostgreSqlAdapter))

  private val databasesSlick: scala.collection.mutable.Map[String, scala.slick.jdbc.JdbcBackend#DatabaseDef] =scala.collection.mutable.Map.empty

  private def database(driverClass:String,ds:DataSource):scala.slick.jdbc.JdbcBackend#DatabaseDef={
    driver match {
      case "org.h2.Driver"=>
        import scala.slick.driver.H2Driver.simple._
        Database.forDataSource(ds)
      case "oracle.jdbc.OracleDriver"=>
        import com.typesafe.slick.driver.oracle.OracleDriver.simple._
        Database.forDataSource(ds)
      case "org.postgresql.Driver"=>
        import scala.slick.driver.PostgresDriver.simple._
        Database.forDataSource(ds)
      case _=>
        import scala.slick.driver.MySQLDriver.simple._
        Database.forDataSource(ds)
    }
  }


  /**
   * 建立一个命名的连接池
   * @param name   连接池名称
   * @param dbConfig  数据库配置
   */
  def openDB(name: String, dbConfig: Config) = {

    val cpds = new BoneCPDataSource
    val driverClass = dbConfig.getString(driver)
    if (adapterCreator.contains(driverClass))
      poolAdapterCreators(name) = adapterCreator(driverClass)
    else
      throw new Exception(s"driver $driverClass not support")

    cpds.setDriverClass(dbConfig.getString(driver))
    cpds.setJdbcUrl(dbConfig.getString(url))
    cpds.setUser(dbConfig.getString(user))
    cpds.setPassword(dbConfig.getString(password))

    if (dbConfig.hasPath(pool_min))
      cpds.setMinConnectionsPerPartition(dbConfig.getInt(pool_min))
    else cpds.setMinConnectionsPerPartition(2)

    if (dbConfig.hasPath(pool_max))
      cpds.setMaxConnectionsPerPartition(dbConfig.getInt(pool_max))
    else cpds.setMaxConnectionsPerPartition(20)

    if (dbConfig.hasPath(pool_inc))
      cpds.setAcquireIncrement(dbConfig.getInt(pool_inc))
    else cpds.setAcquireIncrement(2)

    if (dbConfig.hasPath(pool_partitionCount))
      cpds.setPartitionCount(dbConfig.getInt(pool_partitionCount))
    else cpds.setPartitionCount(2)

    if (dbConfig.hasPath(pool_idleMaxAge)) {
      cpds.setIdleMaxAgeInSeconds(dbConfig.getLong(pool_idleMaxAge))
    }
    if (dbConfig.hasPath(pool_idleConnectionTestPeriod)) {
      cpds.setIdleConnectionTestPeriodInSeconds(dbConfig.getLong(pool_idleConnectionTestPeriod))
    }
    if (dbConfig.hasPath(pool_statementsCacheSize)) {
      cpds.setStatementsCacheSize(dbConfig.getInt(pool_statementsCacheSize))
    }

    pools(name) = cpds
    databasesSlick(name)=database(driverClass,cpds)

    sessionFactories(name) = new SessionFactory {
      def newSession = connection(name)
    }
    val i = 0
  }

  def sessionFactory(name: String) = {
    if (!sessionFactories.contains(name))
      throw new Exception(s"database $name not exist!")
    sessionFactories(name)
  }

  def connection(name: String) = {
    if (!pools.contains(name))
      throw new Exception(s"database $name not exist!")
    val adapterCreator = poolAdapterCreators(name)
    Session.create(pools(name).getConnection, adapterCreator.apply())
  }

  /*获取 sclick db操作对象*/
  def databaseSlick(name:String)={
    if (!databasesSlick.contains(name))
      throw new Exception(s"database $name not exist!")
    databasesSlick(name)
  }

  def hasDB(name: String) = pools.contains(name)

  def closeDB(name: String) = {

    if (pools.contains(name)) {
      pools(name).close()
      pools.remove(name)
      poolAdapterCreators.remove(name)
      databasesSlick.remove(name)
      sessionFactories.remove(name)
    }
  }
  def closeAllDB() {
    pools.keySet.map(closeDB)
  }
}
