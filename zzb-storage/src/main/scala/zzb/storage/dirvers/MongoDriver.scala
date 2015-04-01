package zzb.storage.dirvers

import akka.event.LoggingAdapter
import com.mongodb.casbah.Imports
import com.mongodb.util.JSON
import spray.json._
import zzb.datatype._
import zzb.db.MongoAccess
import zzb.storage.{DBObjectHelper, Driver, TStorable}
import zzb.util.log.Clock._


/**
 * Created by Rowe.Luo on 2014/4/21
 * mongodb数据驱动，主要用于测试
 * @param delay 读写操作延迟的毫秒数，默认为0，不延迟
 */
abstract class MongoDriver[K, KT <: DataType[K], T <: TStorable[K, KT]](delay: Int = 0) extends Driver[K, KT, T] with MongoAccess with DBObjectHelper {

  import com.mongodb.casbah.Imports._

  implicit def theLog: LoggingAdapter = logger

  val del_flag = "sys_del_flag"

  def dbname: String

  /**
   * 指定主键的下一个版本号，从 1 开始。每次调用返回下一个可用的版本，版本号返回后就认为已被使用。
   * 永远自增
   * @param key 数据主键
   * @return
   */
  def nextVerNum(key: K): Int = this.currentVersion(key).fold(1)(f => f(VersionInfo.ver()).get.value + 1)

  /**
   * 保存数据。每一个主键都有一个多个版本的列表。replace 为 true 时用提供的版本覆盖数据库中的当前最新版本。
   * 当 replace 为false 时数据库中的当前最新版本被保留，参数中提供的版本作为一个新的版本实例保存。
   * 保存过程不会修改提供的数据中的版本信息。
   * @param key 主键
   * @param pack 文档数据
   * @param replace 是否覆盖当前数据库版本
   * @return 返回文档自身（数据库操作完成后的最新版本，也就是参数中提供的版本）
   */
  def put(key: K, pack: T#Pack, replace: Boolean = true): T#Pack = {
    val jsonString = pack.toJsValue.toString()
    val dbObject: DBObject = JSON.parse(jsonString).asInstanceOf[DBObject]
    val uuid_key = MongoDBObject(keyCode -> key.toString)
    val cnt = clocking("put时先查询是否已存在{}={}的记录", keyCode, key.toString)(collection(_.getCount(uuid_key)))
    if (cnt == 0) clocking(s"put时插入{}={}的记录", keyCode, key.toString)(collection(_.insert(dbObject)))
    else clocking(s"put时更新{}={}的记录", keyCode, key.toString)(collection(_.update(uuid_key, dbObject)))
    if (!replace) saveHistory(pack)
    pack
  }

  /**
   * 根据指定key装载指定标记的文档。
   * 文档中 verInfo->tag 字段保存着文档的 tag 标签，装载对应标签的文档。
   * @param key 主键
   * @param tag 标签
   * @return 文档
   */
  def load(key: K, tag: String): Option[T#Pack] = {
    val uuid_key = MongoDBObject(keyCode -> key.toString, versionTag -> tag)
    clocking(s"load加载{}={}的记录", keyCode, key.toString)(collection(c => convertDBObject(c.findOne(uuid_key))))
  }

  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param verNum 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, verNum: Int): Option[T#Pack] = {
    if (verNum < 0) {
      val uuid_key = MongoDBObject(keyCode -> key.toString)
      clocking(s"load加载{}={}的最新版本记录", keyCode, key.toString)(collection(c => convertDBObject(c.findOne(uuid_key))))
    } else {
      val uuid_key = MongoDBObject(keyCode -> key.toString, versionCode -> verNum)
      clocking(s"load加载{}={}的历史版本{}记录", keyCode, key.toString, verNum)(historyCollection(c => convertDBObject(c.findOne(uuid_key))))
    }
  }

  /**
   * 获取下一个版本号
   * @param key 主键
   * @return 下一个版本号
   */
  private def nextVersionNum(key: K) = this.currentVersion(key).fold(1)(f => f(VersionInfo.ver()).get.value + 1)

  private def keyCode = docType.keyType.t_code_

  private def versionCode = VersionInfo.t_code_ + "." + Ver.t_code_

  private def versionTag = VersionInfo.t_code_ + "." + Tag.t_code_

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
    clocking(s"delete删除{}={}的记录，标记：{}", keyCode, key.toString, justMarkDelete)(collection { c =>
      c.findOne(uuid_key, data_key).fold(0) { v =>
        doSetDelFlag(key)
        if (!justMarkDelete) {
          doRemove(key)
        }
        1
      }
    }
    )
  }

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
    val version_docs = clocking(s"versions获取{}={}的历史版本", keyCode, key.toString)(historyCollection(_.find(uuid_key, data_key)))

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
    clocking(s"currentVersion获取{}={}的当前版本", keyCode, key.toString)(collection(_.findOne(uuid_key, data_key)) match {
      case None =>
        val seq_versions = versions(key)
        logger.debug(s"currentVersion doc ${key.toString},found None,seq_versions:${seq_versions.length}")
        if (seq_versions.length > 0) Some(seq_versions.last) else None
      case Some(v) =>
        logger.debug(s"currentVersion doc ${key.toString},found :${v.toString}")
        val jsonString = JSON.serialize(v.get(VersionInfo.t_code_))
        val jsonObject: JsValue = JsonParser(jsonString)
        Some(VersionInfo.fromJsValue(jsonObject))
    })
  }

  /**
   * 根据路径、值查询doc列表
   **/
  def find(params: (StructPath, Any)*): List[T#Pack] = {
    clocking(s"find根据路径、值查询doc列表")(collection { col =>
      val p = params.foldLeft(MongoDBObject()) { (result, param) =>
        result ++ (param._1.relativeStr.drop(1).replace("/", ".") -> param._2)
      }
      col.find(p).map { v =>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    })
  }

  override def query(params: (StructPath, Any, String)*): List[T#Pack] = {

    val paramsNew = params.map {
      v =>
        makeFilterByConditionName(v._1, v._2, v._3)
    }
    val filter = makeAndFilter(paramsNew.flatten.toList)
    query(filter)
  }

  /**
   * 根据查询条件集去查讯数据库，取回list
   * @param dbObjcet 查询的条件
   * @return
   */
  override def query(dbObjcet: Imports.DBObject): List[T#Pack] = {
    clocking(s"query根据查询条件集去查讯数据库取回列表")(collection { col =>
      col.find(dbObjcet).map { v =>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    })
  }

  /**
   * 根据查询条件集去查讯数据库符合的条数
   * @param dbObjcet 查询的条件
   * @return
   */
  override def count(dbObjcet: Imports.DBObject): Int = {
    clocking(s"count根据查询条件集去查讯数据库符合的条数")(collection { col =>
      col.count(dbObjcet)
    })
  }

  /**
   * 根据查询条件集去查讯数据库，取回list
   * @param dbObjcet 查询条件
   * @param limit 取回条数
   * @param skip 从第几条开始
   * @return
   */
  override def queryWithLimit(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0): List[T#Pack] = {
    clocking(s"queryWithLimit")(collection { col =>
      col.find(dbObjcet).limit(limit).skip(skip).map { v =>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    })
  }

  override def queryWithLimitSort(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0, sort: Imports.DBObject): List[T#Pack] = {
    clocking(s"queryWithLimitSort")(collection { col =>
      col.find(dbObjcet).sort(sort).limit(limit).skip(skip).map { v =>
        v.removeField("_id")
        v.removeField(del_flag)
        docType.fromJsValue(JsonParser(JSON.serialize(v))).asInstanceOf[T#Pack]
      }.toList
    })
  }


  private def historyCollection[M](f: MongoCollection => M) = {
    db(dbname) { d =>
      f(d(docType.t_code_ + "_history"))
    }
  }

  private def collection[M](f: MongoCollection => M): M = {
    db(dbname) { d =>
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
    clocking(s"持久化${keyCode}=${dbObject.getOrElse(keyCode, "")}历史")(historyCollection(_.insert(dbObject)))
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