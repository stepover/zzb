package zzb.db

import com.typesafe.scalalogging.slf4j.Logging
import com.mongodb.casbah._
import com.typesafe.config.Config

/**
 * mongodb连接、关闭
 * Created by blackangel on 2014/7/18.
 */
object Mongos extends Logging {
  private val dbs: scala.collection.mutable.Map[String, (MongoClient,MongoDB)] = scala.collection.mutable.Map.empty

  /**
   * 连接一个mongodb数据库
   * uri:  mongodb://[username:password@]host1[:port1][,host2[:port2],...[,hostN[:portN]]][/[database][?options]]
   * 连接配置：http://docs.mongodb.org/manual/reference/connection-string/#connection-string-options
   * */
  def openDB(name: String, dbConfig: Config){
    require(dbConfig.hasPath("db"))
    val client: MongoClient =
      if (dbConfig.hasPath("uri"))
        MongoClient(MongoClientURI(dbConfig.getString("uri")))
      else
        MongoClient(MongoClientURI("mongodb://localhost:27017/"))
    dbs(name.toLowerCase)=(client,client(dbConfig.getString("db")))
  }

  def closeDB(name: String) = {

    if (dbs.contains(name.toLowerCase)) {
      dbs(name.toLowerCase)._1.close()
      dbs.remove(name.toLowerCase)
    }
  }
  def closeAllDB() {
    dbs.keySet.map(f=>closeDB(f))
  }

  def _db(name:String)={
    if(!dbs.contains(name.toLowerCase))
      throw new Exception(s"mongo database $name not exist!")
    dbs(name.toLowerCase)._2
  }
}
