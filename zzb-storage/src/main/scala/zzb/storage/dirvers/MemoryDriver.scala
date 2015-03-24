package zzb.storage.dirvers

import java.util.Comparator

import akka.util.Index
import org.joda.time.DateTime
import spray.json._
import zzb.datatype._
import zzb.storage.{KeyNotFountException, DuplicateTagException, Driver, TStorable}

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

  protected final val datas = new Index[K, T#Pack](100, new Comparator[T#Pack] {
    override def compare(o1: T#Pack, o2: T#Pack): Int = o2.version.compareTo(o1.version) //保证降序排列
  })


  implicit def versionListWrap(verList: VersionList) = VersionListWrap(verList)

  case class VersionListWrap(verList: VersionList) {
    def toJsValue = JsArray(verList.map(_.toJsValue))
  }

  private def verListFromJsValue(js: JsValue): VersionList = js match {
    case JsArray(elements) =>
      elements.map(VersionInfo.fromJsValue).toList

    case x => deserializationError("Expected Map as JsObject, but got " + x)
  }

  //case class ID(key: K, ver: Int)

  /**
   * 保存所有文档内容的Map(key 为内容主键和版本对，数据为内容)
   */
  //var db = mutable.Map[ID, T#Pack]()
  //var db = mutable.Map[ID, JsValue]()

  /**
   * 保存文档版本信息的Map(key 为文档内容主键，数据为文档版本列表，新版本在前)
   */
  //var vb = mutable.Map[K, VersionList]()
  //var vb = mutable.Map[K, JsValue]()

  /**
   * 记录每一个主键已经使用过的最大版本号，不存在的主键，版本号为0
   */
  //var nb = mutable.Map[K, Int]()

  /**
   * 记录标记为已删除的文档主键
   */
  var mb = mutable.Set[K]()

  /**
   * 保存文档新版本。提供的文档数据中的 tag 为 tagA,参数中newTag 为 tagB
   *
   * (tagA,tabB) match {
   * case (None,None) => pack 版本号+1, 覆盖当前数据库中的文档
   * case (None,Some(tb)) => pack 版本号+1,tag 设置为tb 覆盖当前数据库中的文档
   * case (Some(ta),None) => pack 版本号+1,tag 设置为"",数据库中增加一个新版本实例，原来的版本不变
   * case (Some(ta),Some(tb) => pack 版本号+1,tag 设置为tb,数据库中增加一个新版本实例，原来的版本不变
   *
   * 所谓版本号 +1 就是 “将参数中的pack版本号设置为数据库中的当前版本号+1”
   *
   *
   * @param pack 文档数据
   * @param operatorName 操作者名称
   * @param isOwnerOperate 是否文档所有人
   * @return 返回数据库中的最新版本
   */
  def save(pack: T#Pack, operatorName: String, isOwnerOperate: Boolean, newTag: String = ""): T#Pack = {
    val key = getKey(pack)
    import zzb.datatype.VersionInfo._

    def firstSave: T#Pack = {
      val newVer = VersionInfo(
        ver := 1,
        time := DateTime.now,
        opt := operatorName,
        isOwn := isOwnerOperate,
        eqtag := "",
        VersionInfo.tag := newTag
      )
      val savedPack = (pack <~ newVer).copy(revise = 0) //修订号清零
      datas.put(key, savedPack)
      savedPack
    }

    def updateSave(nd: T#Pack,od: T#Pack) = {
      val newVer = VersionInfo(
        ver := od.version + 1,
        time := DateTime.now,
        opt := operatorName,
        isOwn := isOwnerOperate,
        eqtag := "", //只要是更新新数据，就不能认为跟之前的某个tag相等了
        VersionInfo.tag := newTag
      )
      val savedPack = (pack <~ newVer).copy(revise = 0) //修订号清零
      datas.remove(key,od)
      datas.put(key, savedPack)
      if(newTag != ""){
        val latest = savedPack <~: savedPack(VersionInfo) <~: List( Ver(savedPack.version + 1), Tag(""),EqTag(newTag))
        datas.put(key,latest)
        latest
      }else
        savedPack
    }

    val vit = datas.valueIterator(key)
    if (vit.isEmpty)
      firstSave
    else
      updateSave(pack,vit.next())
  }

  /**
   * 将当前的数据打上标签，版本固定。同时复制一个版本号+1的新版本(tag为空,eqtag = newTag)作为最新的版本
   * @param key 主键
   * @param newTag 标签
   * @return 返回 tag 为空的最新版本
   */
  def tag(key: K, newTag: String): T#Pack = {
    val vit = datas.valueIterator(key)
    if (vit.isEmpty) throw  KeyNotFountException(key.toString)
    else{
      val od = vit.next()
      val taged = od <~: od(VersionInfo) <~: List(Ver(od.version + 1),Tag(newTag))
      datas.remove(key,od)
      datas.put(key,taged)

      val latest = taged <~: taged(VersionInfo) <~: List( Ver(taged.version + 1), Tag(""),EqTag(newTag))
      datas.put(key,latest)
      latest
    }
  }

  /**
   * 根据指定key装载指定标记的文档
   * @param key 主键
   * @param tag 标签
   * @return 文档
   */
  def load(key: K, tag: String): Option[T#Pack] = {
    datas.findValue(key)(_.tag == tag)
  }

  /**
   * 装载指定主键，指定版本的文档
   * @param key 主键
   * @param verNum 版本号，小于0表示获取最新版
   * @return 文档
   */
  def load(key: K, verNum: Int): Option[T#Pack] = {
    if (verNum >= 0) {
      //指定版本时无论是否标记删除都装载
      if (delay > 0) Thread.sleep(delay)
      datas.findValue(key)(_.version == verNum)
    }
    else {
      if (mb.contains(key)) None //装最新版时检查是否已经标记删除
      else {
        val vit = datas.valueIterator(key)
        if(vit.isEmpty)
          None
        else{
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
  def versions(key: K): Seq[VersionInfo.Pack] = datas.values.map(pack =>
    pack(VersionInfo).get.asInstanceOf[VersionInfo.Pack]).toSeq

  /**
   * 删除指定文档
   * @param key 主键
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  override def delete(key: K, justMarkDelete: Boolean): Int = {
    val vit = datas.valueIterator(key)
    (justMarkDelete,vit.nonEmpty ) match {
      case (_, false) => 0  //指定的key不存在
      case (true,true) => mb.add(key); 1 //标记删除
      case (false,true) => datas.remove(key);mb.remove(key);1 //真正删除
    }
  }


  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param key 主键
   * @param targetVer 旧版本号
   * @return 新文档，如果没有找到指定版本的文档则返回None
   */
  override def revert(key: K, targetVer: Int): Option[T#Pack] = {
    import zzb.datatype.VersionInfo._
    val vit = datas.valueIterator(key)
    if(vit.isEmpty) None
    else{
      val latest = vit.next()
      if(latest.version <= targetVer ) Some(latest)
      else{
        datas.findValue(key)(_.version == targetVer) match {
          case None => None
          case Some(old) =>
            Some(save(old, old(opt).get.value, old(isOwn).get.value))
        }
      }
    }
  }

  /**
   * 恢复文档的指定tag，复制指定的 tag 新建一个新版本，版本号增加
   * @param key 主键
   * @param targetTag 旧tag
   * @return 新文档，如果没有找到指定版本的文档则返回None
   */
  def revert(key: K, targetTag: String): Option[T#Pack] = {
    val vit = datas.valueIterator(key)
    if(vit.isEmpty) None
    else{
      val latest = vit.next()
      if(latest.eqtag == targetTag ) Some(latest)
      else{
        datas.findValue(key)(_.tag == targetTag) match {
          case None => None
          case Some(old) =>
            val reverted = old <~: old(VersionInfo) <~: List(Ver(latest.version + 1), Tag(""),EqTag(targetTag))
            datas.remove(key,latest)
            datas.put(key, reverted)
            Some(reverted)
        }
      }
    }
  }


  /**
   * 根据路径、值查询最新版本文档列表
   * @param params 路径，值键值对参数序列
   **/
  def find(params: (StructPath, Any)*): List[T#Pack] = {
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
    datas.keys.map(k => datas.valueIterator(k).next()).filter(doc => query(doc)).toList
  }
}
