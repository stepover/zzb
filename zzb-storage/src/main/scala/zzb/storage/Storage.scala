package zzb.storage

import akka.event.{NoLogging, LoggingAdapter}
import com.mongodb.casbah.Imports
import org.joda.time.DateTime
import spray.caching.SimpleLruCache
import zzb.datatype._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
 * Created by Simon on 2014/3/27
 *
 * 支持多版本，带缓存功能的数据存储
 *
 * 缓存只保存最新版本
 */
class Storage[K, KT <: DataType[K], T <: TStorable[K, KT]](val driver: Driver[K, KT, T], val maxCache: Int = 1000, val initCache: Int = 50) {

  val inCache = new SimpleLruCache[T#Pack](maxCache, initCache)

  def apply(key: K, ver: Int = -1)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = load(key, ver)


  private def innerSave(key: K, pack: T#Pack, operatorName: String, isOwnerOperate: Boolean, newTag: String = ""): T#Pack = {
    import zzb.datatype.VersionInfo._

    val nextVerNum = driver.nextVerNum(key)
    val firstSave = nextVerNum == 1
    val newVer = VersionInfo(
      ver := nextVerNum,
      time := DateTime.now,
      opt := operatorName,
      isOwn := isOwnerOperate,
      eqtag := "",
      VersionInfo.tag := newTag
    )
    //val savedPack = (pack <~ newVer).copy(revise = 0) //修订号清零
    val savedPack = driver.put(key, (pack <~ newVer).copy(revise = 0), !firstSave)
    if (!firstSave && newTag != "") {
      val latest = (savedPack <~: savedPack(VersionInfo) <~: List(Ver(driver.nextVerNum(key)), Tag(""), EqTag(newTag))).copy(revise = 0)
      driver.put(key, latest, replace = false)
    } else
      savedPack
  }

  private def innerRevert(key: K, targetVer: Int): Option[T#Pack] = driver.load(key) match {
    case None => None
    case Some(latest) =>
      if (latest.version <= targetVer) Some(latest)
      else {
        driver.load(key, targetVer) match {
          case None => None
          case Some(old) =>
            val reverted = (old <~: old(VersionInfo) <~: List(Ver(driver.nextVerNum(key)), Tag(""), EqTag(old.eqtag))).copy(revise = 0)
            Some(driver.put(key, reverted, replace = true))
        }
      }
  }

  /**
   * 恢复文档的指定tag，复制指定的 tag 新建一个新版本，版本号增加
   * @param key 主键
   * @param targetTag 旧tag
   * @return 新文档，如果没有找到指定版本的文档则返回None
   */
  private def innerRevert(key: K, targetTag: String): Option[T#Pack] = driver.load(key) match {
    case None => None
    case Some(latest) =>
      if (latest.eqtag == targetTag) Some(latest)
      else {
        driver.load(key, targetTag) match {
          case None => None
          case Some(taged) =>
            val reverted = (taged <~: taged(VersionInfo) <~: List(Ver(driver.nextVerNum(key)), Tag(""), EqTag(targetTag))).copy(revise = 0)
            Some(driver.put(key, reverted, replace = true))
        }
      }

  }

  /**
   * 从获取指定 key 的对象，如果缓存中没有找到会从 driver 中加载。若指定 onlyInCache 为true，则只从缓存加载。
   * @param key  数据主键
   * @param ver 版本号， 小于0表示最新的版本
   * @param ec 执行上下文
   * @return  查询到的数据(Future)
   */
  def load(key: K, ver: Int = -1,forceReload:Boolean = false)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = {

    val promise = Promise[Option[T#Pack]]()

    if (ver < 0) {
      //寻找最近版本
      if(forceReload) inCache.remove(key)
      val fv: Future[T#Pack] = inCache.get(key) match {
        case Some(f) => f
        case None => inCache.apply(key, () => Future {
          driver.load(key) match {
            case Some(v) => v
            case None => null
          }
        })
      }
      fv.onComplete {
        case Success(v) => promise.success(Option(v))
        case Failure(ex) => promise.failure(ex)
      }
    } else {
      //寻找指定版本，不使用缓存
      Future {
        driver.load(key, ver)
      }.onComplete {
        case Success(v) => promise.success(v)
        case Failure(ex) => promise.failure(ex)
      }
    }

    promise.future
  }

  def load(key: K, tag: String)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = Future {
    driver.load(key, tag)
  }

  /**
   * 保存文档新版本，如果指定了tag,将新保存的数据打上标签，版本固定。
   * 同时复制一个版本号+1的新版本(tag为空)作为最新的版本.
   * 如果指定 tag,就修改当前最新版本的内容,版本号会+1,但是不会克隆新的副本，不保存旧版本的文档数据
   * @param pack 文档数据
   * @param operatorName 操作者名称
   * @param isOwnerOperate 是否文档所有人
   * @return 更新了版本好的新文档，如果指定了tag,返回 tag 为空的最新版本
   */
  def save(pack: T#Pack, operatorName: String = "", isOwnerOperate: Boolean = true, newTag: String = "")(implicit ec: ExecutionContext): Future[T#Pack] = {
    require(newTag ne null)
    val key = driver.getKey(pack)
    inCache.remove(key)
    inCache.apply(key, () => Future {
      innerSave(key, pack, operatorName, isOwnerOperate, newTag)
    })
  }

  /**
   * 将当前的数据打上标签，版本固定。同时复制一个版本号+1的新版本(tag为空)作为最新的版本
   * @param key 主键
   * @param newTag 标签
   * @return 返回 tag 为空的最新版本
   */
  def tag(key: K, newTag: String)(implicit ec: ExecutionContext): Future[T#Pack] = {
    require(newTag ne null)
    inCache.remove(key)
    inCache.apply(key, () => Future {
      innerTag(key, newTag)
    })
  }

  private def innerTag(key: K, newTag: String): T#Pack = driver.load(key) match {
    case None => throw KeyNotFountException(key.toString)
    case Some(latest) =>
      val taged = driver.put(key,
        (latest <~: latest(VersionInfo) <~: List(Ver(driver.nextVerNum(key)), Tag(newTag))).copy(revise = 0),
        replace = true
      )
      driver.put(key,
        (taged <~: taged(VersionInfo) <~: List(Ver(driver.nextVerNum(key)), Tag(""), EqTag(newTag))).copy(revise = 0),
        replace = false
      )
  }


  //  def save(pack: T#Pack, tag:String = "")(implicit ec: ExecutionContext): Future[T#Pack] = {
  //    save(pack,"",isOwnerOperate = true,tag)
  //  }
  /**
   * 获取最近的几个版本信息，最新的在前面
   * @param key 主键
   * @return 文档版本信息列表
   */
  def versions(key: K)(implicit ec: ExecutionContext): Future[Seq[VersionInfo.Pack]] = Future {
    driver.versions(key)
  }

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  def delete(key: K, justMarkDelete: Boolean = true)(implicit ec: ExecutionContext): Future[Int] = Future {
    val res = driver.delete(key, justMarkDelete)
    if (res > 0) inCache.remove(key)
    res
  }

  /**
   * 将指定文档从缓存中释放
   * @param key   主键
   * @param ec
   * @return  已经被找到并删除的文档
   */
  def release(key: K)(implicit ec: ExecutionContext) = inCache.remove(key) match {
    case Some(f) => f.map(d => Some(d))
    case None => Future(None)
  }

  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param key 主键
   * @param targetVer 旧版本号
   * @return 新文档
   */
  def revert(key: K, targetVer: Int)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = {
    val promise = Promise[Option[T#Pack]]()

    //寻找指定版本，不使用缓存
    Future {
      innerRevert(key, targetVer)
    }.onComplete {
      case Success(ov) =>
        ov match {
          case Some(v) => //更新缓存
            inCache.remove(key)
            inCache.apply(key, () => Future(v))
          case None => ()
        }
        promise.success(ov)
      case Failure(ex) => promise.failure(ex)
    }
    promise.future
  }

  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param key 主键
   * @param targetTag 旧版本号
   * @return 新文档
   */
  def revert(key: K, targetTag: String)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = {
    val promise = Promise[Option[T#Pack]]()

    //寻找指定版本，不使用缓存
    Future {
      innerRevert(key, targetTag)
    }.onComplete {
      case Success(ov) =>
        ov match {
          case Some(v) => //更新缓存
            inCache.remove(key)
            inCache.apply(key, () => Future(v))
          case None => ()
        }
        promise.success(ov)
      case Failure(ex) => promise.failure(ex)
    }
    promise.future
  }


  /**
   * 根据路径、值查询最新版本文档列表
   **/
  def query(params: List[(StructPath, Any, String)])(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    val promise = Promise[List[T#Pack]]()

    Future {
      try {
        promise.success(driver.query(params))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }

  /**
   * 根据路径、值查询最新版本文档列表
   **/
  def query(dbObjcet: Imports.DBObject)(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    val promise = Promise[List[T#Pack]]()

    Future {
      try {
        promise.success(driver.query(dbObjcet))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }

  def queryWithLimit(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0)(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    val promise = Promise[List[T#Pack]]()

    Future {
      try {
        promise.success(driver.queryWithLimit(dbObjcet, limit, skip))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }

  def queryWithLimitSort(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0, sort: Imports.DBObject)(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    val promise = Promise[List[T#Pack]]()

    Future {
      try {
        promise.success(driver.queryWithLimitSort(dbObjcet, limit, skip, sort))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }

  def count(dbObjcet: Imports.DBObject)(implicit ec: ExecutionContext): Future[Int] = {
    val promise = Promise[Int]()

    Future {
      try {
        promise.success(driver.count(dbObjcet))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }


  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对参数序列
   **/
  def find(params: (StructPath, Any)*)(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    val promise = Promise[List[T#Pack]]()

    Future {
      try {
        promise.success(driver.find(params: _*))
      }
      catch {
        case e: Throwable =>
          promise.failure(e)
      }
    }
    promise.future
  }

  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对列表
   **/
  def find(params: List[(StructPath, Any)])(implicit ec: ExecutionContext): Future[List[T#Pack]] = {
    find(params: _*)
  }

  def cacheClear() = inCache.clear()

  def cacheSize = inCache.size

  def specific(key: K) = new SpecificStorage[K, KT, T](key, this)

}

class SpecificStorage[K, KT <: DataType[K], T <: TStorable[K, KT]](val key: K, val storage: Storage[K, KT, T]) {
  def apply(ver: Int = -1,forceReload:Boolean = false)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = storage.load(key, ver,forceReload)

  def apply(tag: String)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = storage.load(key, tag)

  def load(ver: Int = -1)(implicit ec: ExecutionContext) = storage.load(key, ver)

  def save(pack: T#Pack, operatorName: String = "", isOwnerOperate: Boolean = true, newTag: String = "")(implicit ec: ExecutionContext) = {
    require(pack(pack.dataType.asInstanceOf[TStorable[K, KT]].keyType).get.value == key)
    storage.save(pack, operatorName, isOwnerOperate, newTag)
  }

  /**
   * 将当前的数据打上标签，版本固定。同时复制一个版本号+1的新版本(tag为空)作为最新的版本
   * @param newTag 标签
   * @return 返回 tag 为空的最新版本
   */
  def tag(newTag: String)(implicit ec: ExecutionContext): Future[T#Pack] = {
    storage.tag(key, newTag)
  }


  def versions(implicit ec: ExecutionContext) = storage.versions(key)

  /**
   * 删除文档
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  def delete(justMarkDelete: Boolean)(implicit ec: ExecutionContext): Future[Int] = storage.delete(key, justMarkDelete)

  /**
   * 将指定文档从缓存中释放
   * @param ec
   * @return  已经被找到并删除的文档
   */
  def release()(implicit ec: ExecutionContext) = storage.release(key)

  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param targetVer 旧版本号
   * @return 新文档
   */
  def revert(targetVer: Int)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = storage.revert(key, targetVer)

  /**
   * 恢复文档的指定Tag版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param targetTag 旧版本号
   * @return 新文档
   */
  def revert(targetTag: String)(implicit ec: ExecutionContext): Future[Option[T#Pack]] = storage.revert(key, targetTag)
}


trait Driver[K, KT <: DataType[K], T <: TStorable[K, KT]] {

  val docType: T

  val logger : LoggingAdapter = NoLogging

  def getKey(pack: T#Pack): K = {
    pack(pack.dataType.asInstanceOf[TStorable[K, KT]].keyType).get.value
  }

  def nextVerNum(key: K): Int

  def put(key: K, pack: T#Pack, replace: Boolean ): T#Pack

  /**
   * 根据指定key装载版本最新文档
   * @param key 主键
   * @return 文档
   */
  def load(key: K): Option[T#Pack] = load(key, -1)

  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param ver 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, ver: Int): Option[T#Pack] //Some(docType.fromJsValue(JsString("")).asInstanceOf[T#Pack])

  /**
   * 根据指定key装载指定标记的文档
   * @param key 主键
   * @param tag 标签
   * @return 文档
   */
  def load(key: K, tag: String): Option[T#Pack]


  /**
   * 获取最近的几个版本信息，最新的在前面
   * @param key 主键
   * @return 文档版本信息列表
   */
  def versions(key: K): Seq[VersionInfo.type#Pack]

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  def delete(key: K, justMarkDelete: Boolean): Int

  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对参数序列
   **/
  def find(params: (StructPath, Any)*): List[T#Pack]

  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对列表
   **/
  def find(params: List[(StructPath, Any)]): List[T#Pack] = {
    find(params: _*)
  }

  def query(params: (StructPath, Any, String)*): List[T#Pack] = List.empty

  def query(params: List[(StructPath, Any, String)]): List[T#Pack] = {
    query(params: _*)
  }

  def query(dbObjcet: Imports.DBObject): List[T#Pack] = List.empty

  def queryWithLimit(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0): List[T#Pack] = List.empty

  def queryWithLimitSort(dbObjcet: Imports.DBObject, limit: Int = 10, skip: Int = 0, sort: Imports.DBObject): List[T#Pack] = List.empty

  def count(dbObjcet: Imports.DBObject): Int = 0

}

case class DuplicateTagException(key: String, tag: String) extends Exception(s"DuplicateTag '$tag' for key '$key' ")

case class KeyNotFountException(key: String) extends Exception(s"Not fount key '$key'")
