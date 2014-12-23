package zzb.storage.dirvers

import com.mongodb.casbah.Imports
import com.mongodb.util.JSON
import zzb.datatype.{StructPath, Del, VersionInfo, DataType}
import zzb.storage.{TStorable, Driver}
import org.joda.time.DateTime
import scala.collection.mutable
import spray.json._
import scala.Some


/**
 * Created by Simon on 2014/3/31
 */

/**
 * 内存数据驱动，主要用于测试
 * @param delay 读写操作延迟的毫秒数，默认为0，不延迟
 */
abstract class MemoryDriver[K, KT <: DataType[K], T <: TStorable[K, KT]](delay: Int = 0) extends Driver[K, KT, T] {

  type VersionList = List[VersionInfo.Pack]

  implicit def versionListWrap(verList: VersionList) = VersionListWrap(verList)

  case class VersionListWrap(verList: VersionList) {
    def toJsValue = JsArray(verList.map(_.toJsValue))
  }

  private def verListFromJsValue(js: JsValue): VersionList = js match {
    case JsArray(elements) =>
      elements.map(VersionInfo.fromJsValue).toList

    case x => deserializationError("Expected Map as JsObject, but got " + x)
  }

  /**
   * 多版本文档内容集合主键
   * @param key 数据主键
   * @param ver 数据主键版本
   */
  case class ID(key: K, ver: Int)

  /**
   * 保存所有文档内容的Map(key 为内容主键和版本对，数据为内容)
   */
  //var db = mutable.Map[ID, T#Pack]()
  var db = mutable.Map[ID, JsValue]()

  /**
   * 保存文档版本信息的Map(key 为文档内容主键，数据为文档版本列表，新版本在前)
   */
  //var vb = mutable.Map[K, VersionList]()
  var vb = mutable.Map[K, JsValue]()

  /**
   * 记录每一个主键已经使用过的最大版本号，不存在的主键，版本号为0
   */
  var nb = mutable.Map[K, Int]()

  /**
   * 记录标记为已删除的文档主键
   */
  var mb = mutable.Set[K]()

  /**
   * 获取下一个版本号
   * @param key 主键
   * @return 下一个版本号
   */
  private def nextVersionNum(key: K): Int = {
    val n = nb.getOrElse(key, 0)
    nb(key) = n + 1
    n + 1
  }

  /**
   * 保存文档新版本。
   * @param pack 文档数据
   * @param operatorName 操作者名称
   * @param isOwnerOperate 是否文档所有人
   * @return 更新了版本好的新文档
   */
  def save(pack: T#Pack, operatorName: String, isOwnerOperate: Boolean): T#Pack = {
    val key = getKey(pack)
    import VersionInfo._

    def doSave(verList: VersionList) = {
      val newVerNum = nextVersionNum(key)
      val newVer = VersionInfo(
        ver := newVerNum,
        time := DateTime.now,
        opt := operatorName, isOwn := isOwnerOperate)

      val savedPack = (pack <~ newVer).copy(revise = 0) //修订号清零

      if (delay > 0) Thread.sleep(delay)

      db(ID(key, newVerNum)) = savedPack.toJsValue

      vb(key) = (newVer :: verList).toJsValue

      mb.remove(key) //清除删除标记

      savedPack
    }
    vb.get(key) match {
      case None => doSave(Nil)
      case Some(v) =>
        val verList = verListFromJsValue(v)
        verList match {
          case maxVer :: tail =>
            //            val maxVerNum = maxVer(ver).get.value
            //            if (maxVerNum > pack.version)
            //              docType.fromJsValue(db.get(ID(key, maxVerNum)).get).asInstanceOf[T#Pack] //已存储的版本大于提交存储的版本，返回已存储的版本
            //else
            doSave(maxVer :: tail)
          case Nil => throw new Exception("error")
        }
    }
  }


  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param verNum 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, verNum: Int): Option[T#Pack] = {
    import VersionInfo._
    if (verNum >= 0) { //指定版本时无论是否标记删除都装载
      if (delay > 0) Thread.sleep(delay)
      db.get(ID(key, verNum)).map(docType.fromJsValue(_).asInstanceOf[T#Pack])
    }
    else {
      if (mb.contains(key)) None //装最新版时检查是否已经标记删除
      else vb.get(key) match {
        case None => None
        case Some(v) =>
          val verList = verListFromJsValue(v)
          verList match {
            case maxVer :: tail =>
              db.get(ID(key, maxVer(ver).get.value)).map(docType.fromJsValue(_).asInstanceOf[T#Pack])
            case Nil => None
          }
      }
    }
  }

  /**
   * 获取最近的几个版本信息，最新的在前面
   * @param key 主键
   * @return 文档版本信息列表
   */
  def versions(key: K): Seq[VersionInfo.Pack] = vb.get(key) match {
    case None => Nil
    case Some(v) => verListFromJsValue(v)
  }

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  override def delete(key: K, justMarkDelete: Boolean): Int = (justMarkDelete, vb.contains(key)) match {
    case (_, false) => 0
    case (true, true) => mb.add(key); 1
    case (false, true) =>
      val verList = verListFromJsValue(vb.get(key).get)
      import VersionInfo._
      for (vInfo <- verList) {
        val verNum = vInfo(ver).get.value
        db.remove(ID(key, verNum))
      }
      vb.remove(key)
      nb.remove(key)
      mb.remove(key)
      1
  }


  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param key 主键
   * @param oldVer 旧版本号
   * @return 新文档，如果没有找到指定版本的文档则返回None
   */
  override def revert(key: K, oldVer: Int): Option[T#Pack] = vb.get(key) match {
    case None => None
    case Some(v) =>
      val verList = verListFromJsValue(v)
      import VersionInfo._
      verList match {
        case maxVer :: tail =>
          val maxVerNum = maxVer(ver).get.value
          if (oldVer >= maxVerNum)
            db.get(ID(key, maxVer(ver).get.value)).map(docType.fromJsValue(_).asInstanceOf[T#Pack])
          else {
            load(key, oldVer) match {
              case None => None
              case Some(oldDoc) => Some(save(oldDoc, oldDoc(opt).get.value, oldDoc(isOwn).get.value))
            }
          }
        case Nil => None
      }
  }

  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对参数序列
   **/
  def find(params: (StructPath, Any)*): List[T#Pack]={
    def query =
      (doc:T#Pack) =>{
        params.forall{
          case (path,value)=>
            path.getDomainData(doc).fold(false){
              f=>
                val key= doc(doc.dataType.asInstanceOf[TStorable[K, KT]].keyType).get.value
                val maxV=nb(key)
                val cuV = doc(VersionInfo.ver()).get.value
                f==path.targetType.AnyToPack(value)&& maxV==cuV}
        }
      }
    db.values.map(f=>docType.fromJsValue(f).asInstanceOf[T#Pack]).filter(f=>query(f)).toList
  }

}
