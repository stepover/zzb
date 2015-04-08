package zzb.storage.dirvers

import java.util.Comparator

import akka.event.LoggingAdapter
import akka.util.Index
import zzb.datatype._
import zzb.storage.{Driver, TStorable}
import zzb.util.log.Clock._

import scala.collection.mutable

/**
 * Created by Simon on 2014/3/31
 */

/**
 * 内存数据驱动，主要用于测试
 * @param delay 读写操作延迟的毫秒数，默认为0，不延迟
 */
abstract class MemoryDriver[K, KT <: DataType[K], T <: TStorable[K, KT]](delay: Int = 0) extends Driver[K, KT, T] {

  type VersionList = List[VersionInfo.Pack]
  type MemDB = Index[K, T#Pack]

  protected final val db = new MemDB(100, new Comparator[T#Pack] {
    override def compare(o1: T#Pack, o2: T#Pack): Int = o2.version.compareTo(o1.version) //保证降序排列
  })

  implicit def theLog: LoggingAdapter = logger

  def nextVerNum(key: K): Int = clocking("get nextVerNum") {
    val vit = db.valueIterator(key)
    val verNum = if (vit.hasNext) vit.next().version + 1 else 1
    logger.debug("next ver num:{}", verNum)
    verNum
  }


  def put(key: K, pack: T#Pack, replace: Boolean): T#Pack = clocking("put data key {}", key) {
    val vit = db.valueIterator(key)
    if (vit.hasNext && replace)
      db.remove(key, vit.next())
    db.put(key, pack)
    pack
  }

  /**
   * 记录标记为已删除的文档主键
   */
  var mb = mutable.Set[K]()

  /**
   * 根据指定key装载指定标记的文档
   * @param key 主键
   * @param tag 标签
   * @return 文档
   */
  def load(key: K, tag: String): Option[T#Pack] = clocking("load data key {} tag", key, tag) {
    db.findValue(key)(_.tag == tag)
  }

  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param verNum 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, verNum: Int): Option[T#Pack] = clocking("load data key {} version {}", key, verNum) {
    if (verNum >= 0) {
      //指定版本时无论是否标记删除都装载
      if (delay > 0) Thread.sleep(delay)
      db.findValue(key)(_.version == verNum)
    }
    else {
      if (mb.contains(key)) None //装最新版时检查是否已经标记删除
      else {
        val vit = db.valueIterator(key)
        if (vit.isEmpty)
          None
        else {
          if (delay > 0) Thread.sleep(delay)
          Some(vit.next())
        }
      }
    }
  }

  /**
   * 获取最近的几个版本信息，最新的在前面
   * @param key 主键
   * @return 文档版本信息列表
   */
  def versions(key: K): Seq[VersionInfo.Pack] = clocking("load key {} version list", key) {
    db.values.map(pack =>
      pack(VersionInfo).get.asInstanceOf[VersionInfo.Pack]).toSeq
  }

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  override def delete(key: K, justMarkDelete: Boolean): Int = clocking("delete data key {} ", key) {
    val vit = db.valueIterator(key)
    (justMarkDelete, vit.nonEmpty) match {
      case (_, false) => 0 //指定的key不存在
      case (true, true) => mb.add(key); 1 //标记删除
      case (false, true) => db.remove(key); mb.remove(key); 1 //真正删除
    }
  }

  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对参数序列
   **/
  def find(params: (StructPath, Any)*): List[T#Pack] = clocking("find data params {}",params){
    def query =
      (doc: T#Pack) => {
        params.forall {
          case (path, forCheckValue) =>
            path.getDomainData(doc).fold(false) {
              pathValue =>
                pathValue == path.targetType.AnyToPack(forCheckValue)
            }
        }
      }
    db.keys.map(k => db.valueIterator(k).next()).filter(doc => query(doc)).toList
  }
}
