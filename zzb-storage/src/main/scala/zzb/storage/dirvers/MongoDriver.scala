package zzb.storage.dirvers

import com.mongodb.casbah.{query, Imports}
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.query.dsl.QueryExpressionObject
import zzb.datatype._
import org.joda.time.DateTime
import spray.json._
import zzb.storage.{DBObjectHelper, Driver, TStorable}

import com.mongodb.util.JSON
import scala.Some
import zzb.db.MongoAccess
import com.typesafe.scalalogging.slf4j.Logging

/**
 * Created by Rowe.Luo on 2014/4/21
 * mongodb数据驱动，主要用于测试
 * @param delay 读写操作延迟的毫秒数，默认为0，不延迟
 */
abstract class MongoDriver[K, KT <: DataType[K], T <: TStorable[K, KT]](delay: Int = 0) extends Driver[K, KT, T] with MongoAccess with DBObjectHelper with Logging{

  import com.mongodb.casbah.Imports._

  val del_flag = "sys_del_flag"

  def dbname: String

  def exist(key:K):Boolean = ???

  def nextVerNum(key:K) :Int = ???

  def put(key:K,pack: T#Pack,replace : Boolean = true) : T#Pack = ???

  /**
   * 获取下一个版本号
   * @param key 主键
   * @return 下一个版本号
   */
  private def nextVersionNum(key: K) = this.currentVersion(key).fold(1)(f=>f(VersionInfo.ver()).get.value + 1)

  /**
   * 保存文档新版本
   * @param doc 文档数据
   * @param operatorName 操作者名称
   * @param isOwnerOperate 是否文档所有人
   * @return 更新了版本好的新文档，如果指定了tag,返回 tag 为空的最新版本
   */
  def save(doc: T#Pack, operatorName: String, isOwnerOperate: Boolean,newTag:String = ""): T#Pack = {
    val key = getKey(doc)
    import VersionInfo._

    val newVerNum = nextVersionNum(key)
    val newVer = VersionInfo(
      ver := newVerNum,
      time := DateTime.now,
      opt := operatorName, isOwn := isOwnerOperate)

    val newDoc = doc <~ newVer
    logger.debug(s"save doc #${key.toString},newVer: #${newVerNum.toString}")
    if (delay > 0) Thread.sleep(delay)

    doSave(newVerNum, newDoc)
    saveHistory(newDoc)
    newDoc
  }

  /**
   * 根据指定key装载指定标记的文档
   * @param key 主键
   * @param tag 标签
   * @return 文档
   */
  def load(key: K,tag:String): Option[T#Pack] = ???

  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param verNum 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, verNum: Int): Option[T#Pack] = {
    if (verNum < 0) {
      val uuid_key = MongoDBObject(keyCode -> key.toString)
      collection(c => convertDBObject(c.findOne(uuid_key)))
    } else {
      val uuid_key = MongoDBObject(keyCode -> key.toString, versionCode -> verNum)
      historyCollection(c => convertDBObject(c.findOne(uuid_key)))
    }
  }

  private def keyCode = docType.keyType.t_code_

  private def versionCode = VersionInfo.t_code_ + "." + Ver.t_code_

  private def convertDBObject(dbObject: Option[DBObject]): Option[T#Pack] = {
    dbObject match {
      case Some(v) if !v.get(del_flag).asInstanceOf[Boolean] =>
        import spray.json._
        //TODO MODIFY
        v.removeField("_id")
        v.removeField(del_flag)
        val jsonString = JSON.serialize(v)
        val jsonObject: JsValue = JsonParser(jsonString)
        Some(docType.fromJsValue(jsonObject).asInstanceOf[T#Pack])
      case _ => None
    }
  }

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  def delete(key: K, justMarkDelete: Boolean): Int = {
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    val data_key = MongoDBObject(keyCode -> "1", del_flag -> "1")
    collection { c =>
      c.findOne(uuid_key, data_key).fold(0) { v =>
        doSetDelFlag(key)
        if (!justMarkDelete) {
          doRemove(key)
        }
        1
      }
    }
  }

//  /**
//   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
//   * @param key 主键
//   * @param targetVer 旧版本号
//   * @return 新文档，如果没有找到指定版本的文档则返回None
//   */
//  def revert(key: K, targetVer: Int): Option[T#Pack] = {
//    val currentVer = currentVersion(key) match {
//      case None => -1
//      case Some(cVer) => cVer(VersionInfo.ver()).get.value
//    }
//
//    currentVer match {
//      case -1 => None
//      case `targetVer` =>
//        load(key, targetVer)
//      case _ =>
//        revertHistory(key, targetVer)
//    }
//  }
//
//
//  private def revertHistory(key: K, oldVer: Int): Option[T#Pack] = {
//    load(key, oldVer).map(v => save(v, "", isOwnerOperate = false))
//  }

  /**
   * 获取最近的几个版本信息，最新的在前面
   * @param key 主键
   * @return 文档版本信息列表
   */
  def versions(key: K): Seq[VersionInfo.Pack] = {
    //获取没有删除标志的版本
    //val uuid_key = MongoDBObject(keyCode -> key.toString, VersionInfo.t_code_ + "." + Del.t_code_ -> false)
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    val data_key = MongoDBObject(VersionInfo.t_code_ -> "1")
//    val data_order = MongoDBObject(versionCode -> "desc")
    val version_docs = historyCollection(_.find(uuid_key, data_key))

    import spray.json._
    version_docs.map {
      doc =>
        val jsonString = JSON.serialize(doc.get(VersionInfo.t_code_))
        val jsonObject = JsonParser(jsonString)
        VersionInfo.fromJsValue(jsonObject)
    }.toSeq.sortBy(f => f(VersionInfo.ver()).get.value * -1)
  }

  def currentVersion(key: K): Option[VersionInfo.Pack] = {
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    val data_key = MongoDBObject(VersionInfo.t_code_ -> "1")
    collection(_.findOne(uuid_key, data_key)) match {
      case None =>
        val seq_versions = versions(key)
        logger.debug(s"currentVersion doc ${key.toString},found None,seq_versions:${seq_versions.length}")
        if (seq_versions.length > 0) Some(seq_versions.last) else None
      case Some(v) =>
        logger.debug(s"currentVersion doc ${key.toString},found :${v.toString}")
        val jsonString = JSON.serialize(v.get(VersionInfo.t_code_))
        val jsonObject: JsValue = JsonParser(jsonString)
        Some(VersionInfo.fromJsValue(jsonObject))
    }
  }

  /**
   * 根据路径、值查询doc列表
   **/
  def find(params: (StructPath, Any)*): List[T#Pack] = {
    collection { col =>
      val p = params.foldLeft(MongoDBObject()) { (result, param) =>
        result ++ (param._1.relativeStr.drop(1).replace("/",".") -> param._2)
      }
      col.find(p).map{v=>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    }
  }

  override def query(params: (StructPath, Any,String)*): List[T#Pack]={

    val paramsNew=  params.map{
        v=>
          makeFilterByConditionName(v._1,v._2,v._3)
      }
    val filter= makeAndFilter(paramsNew.flatten.toList)
    query(filter)
  }

  /**
   * 根据查询条件集去查讯数据库，取回list
   * @param dbObjcet 查询的条件
   * @return
   */
  override def query(dbObjcet: Imports.DBObject):List[T#Pack]={
    collection { col =>
      col.find(dbObjcet).map{v=>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    }
  }

  /**
   * 根据查询条件集去查讯数据库符合的条数
   * @param dbObjcet 查询的条件
   * @return
   */
  override def count(dbObjcet: Imports.DBObject):Int={
    collection { col =>

      col.count(dbObjcet)
    }
  }

  /**
   * 根据查询条件集去查讯数据库，取回list
   * @param dbObjcet 查询条件
   * @param limit 取回条数
   * @param skip 从第几条开始
   * @return
   */
  override def queryWithLimit(dbObjcet: Imports.DBObject,limit:Int=10,skip:Int=0):List[T#Pack]={
    collection { col =>
      col.find(dbObjcet).limit(limit).skip(skip).map{v=>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    }
  }
  override def queryWithLimitSort(dbObjcet: Imports.DBObject,limit:Int=10,skip:Int=0 ,sort:Imports.DBObject):List[T#Pack]={
    collection { col =>
      col.find(dbObjcet).sort(sort).limit(limit).skip(skip).map{v=>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    }
  }



  private def historyCollection[M](f: MongoCollection => M) = {
    db(dbname){d=>
      f(d(docType.t_code_ + "_history"))
    }
  }

  private def collection[M](f: MongoCollection => M): M = {
    db(dbname){d=>
      f(d(docType.t_code_))
    }
  }

  private def doSetDelFlag(key: K): Boolean = {
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    val updateDBObject = MongoDBObject("$set" -> MongoDBObject(del_flag -> true))
    collection(_.update(uuid_key, updateDBObject))

    true
  }

  private def doRemove(key: K): Boolean = {
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    collection(_.remove(uuid_key))
    historyCollection(_.remove(uuid_key))
    true
  }

  private def saveHistory(pack: T#Pack): T#Pack = {
    val jsonString = pack.toJsValue.toString()
    val dbObject: DBObject = JSON.parse(jsonString).asInstanceOf[DBObject]
    historyCollection(_.insert(dbObject))
    pack
  }

  private def doSave(newVerNum: Int, pack: T#Pack): T#Pack = {
    val jsonString = pack.toJsValue.toString()
    val dbObject: DBObject = JSON.parse(jsonString).asInstanceOf[DBObject]
    if (newVerNum > 1) {
      val key = getKey(pack)
      val uuid_key = MongoDBObject(keyCode -> key.toString)
      collection(_.update(uuid_key, dbObject))
    } else {
      collection(_.insert(dbObject))
    }
    pack
  }
}