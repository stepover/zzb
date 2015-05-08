package zzb.storage

import zzb.datatype.DataType

import scala.concurrent.stm._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
 * Created by Simon on 2014/4/2
 */
trait DocProcessor[K, KT <: DataType[K], T <: TStorable[K, KT]] {
  val specStorage: SpecificStorage[K, KT, T]
  val isNewCreate: Boolean

  def docExecutionContext: ExecutionContext

  implicit val exe = docExecutionContext

  //  //版本缓存，版本号为key
  //  val verDocs = new ConcurrentLinkedHashMap.Builder[Int, Future[Option[T#Pack]]]
  //    .initialCapacity(3)
  //    .maximumWeightedCapacity(5)
  //    .build()

  /** 当前文档快照（不精确，想要精确的就取latest，它是个Future） */
  //var snapDoc: Option[T#Pack] = None
  val unFlushDoc : Ref[Option[T#Pack]] = Ref(Option[T#Pack](null))

  def createDoc: Future[Option[T#Pack]] //= None

  //最近文档的Future保存在 STM 引用中
  lazy val latest_f: Ref[Future[Option[T#Pack]]] =
    Ref(
      latest_f_init
    )

  def latest_f_init: Future[Option[T#Pack]] = {
    val f = if (isNewCreate)
      createDoc
    else
      specStorage(-1) //不是新建，则从存储中装载
    //    f.onSuccess {
    //      case d => snapDoc = d
    //    }
    f
  }

  //  //执行实际的装载动作，不从版本缓存中装载
  //  private def doReload(verNum: Int = -1): Future[Option[T#Pack]] = {
  //    val promise = Promise[Option[T#Pack]]()
  //    if (verNum < 0) {
  //      val lvd_f = atomic {
  //        implicit txn =>
  //          val lvd_f = specStorage(-1)
  //          latest_f.set(lvd_f)
  //          lvd_f
  //      }
  //      lvd_f.onComplete {
  //        case Success(Some(lvd)) =>
  //          val vn = lvd.version
  //          verDocs.put(vn, lvd_f)
  //          snapDoc = Some(lvd)
  //          promise.success(Some(lvd))
  //        case Success(None) =>
  //          snapDoc = None
  //          promise.success(None)
  //        case Failure(ex) => promise.failure(ex)
  //      }
  //    }
  //    else {
  //      val vd_f = specStorage(verNum)
  //      vd_f.onComplete {
  //        case Success(Some(vd)) => promise.success(Some(vd))
  //        case Success(None) => promise.success(None)
  //        case Failure(ex) => promise.failure(ex)
  //      }
  //      verDocs.put(verNum, promise.future)
  //    }
  //    promise.future
  //  }

  def load(verNum: Int = -1, forceReload: Boolean = false): Future[Option[T#Pack]] = {
    val promise = Promise[Option[T#Pack]]()
    if (verNum < 0) {
      val lvd_f = atomic {
        implicit txn =>
          unFlushDoc.get match {
            case None =>
              val lvd_f = specStorage(-1, forceReload)
              latest_f.set(lvd_f)
              lvd_f
            case Some(doc) => latest_f.get
          }
      }
      lvd_f.onComplete {
        case Success(Some(lvd)) =>
          promise.success(Some(lvd))
        case Success(None) =>
          promise.success(None)
        case Failure(ex) => promise.failure(ex)
      }
    }
    else {
      val vd_f = specStorage(verNum)
      vd_f.onComplete {
        case Success(Some(vd)) => promise.success(Some(vd))
        case Success(None) => promise.success(None)
        case Failure(ex) => promise.failure(ex)
      }
    }
    promise.future
  }

  def load(tag: String): Future[Option[T#Pack]] = {
    specStorage(tag)
  }

  def save(pack: T#Pack, operatorName: String = "", isOwnerOperate: Boolean = true,
           flush: Boolean = true, newTag: String = ""): Future[Option[T#Pack]] = {
    //    snapDoc = Some(pack)
    require(newTag ne null)
    val promise = Promise[Option[T#Pack]]()
    if (!flush && newTag == "") {
      atomic {
        implicit txn => unFlushDoc.set(Some(pack))
      }
      promise.success(Some(pack))
    }
    else {
      val vd_f = specStorage.save(pack, operatorName, isOwnerOperate, newTag)
      vd_f.onComplete {
        case Success(vd) =>
          atomic {
            implicit txn => unFlushDoc.set(None)
          }
          promise.success(Some(vd))
        case Failure(ex) => promise.failure(ex)
      }
    }
    atomic {
      implicit txn => latest_f.set(promise.future)
    }
    promise.future
  }

  def flush() :Future[Option[T#Pack]] = {
    val dirty = atomic {  implicit txn => unFlushDoc.get   }
    dirty match {
      case None => latest
      case Some(doc) => save(doc)
    }
  }

  def tag(newTag: String): Future[T#Pack] = specStorage.tag(newTag)

  def versions = specStorage.versions

  /**
   * 删除文档
   * @param justMarkDelete 是否标记删除
   * @return 删除数量 1 或 0
   */
  def delete(justMarkDelete: Boolean = true): Future[Int] = {
    //snapDoc = None
    val promise = Promise[Int]()
    val count_f = specStorage.delete(justMarkDelete)
    count_f.onComplete {
      case Success(1) =>
        atomic {
          implicit txn =>
            latest_f.set(Future(None)) //更新对最近文档的 STM 引用
            unFlushDoc.set(None)
        }
        promise.success(1)
      case Success(n) => promise.success(n)
      case Failure(e) => promise.failure(e)
    }
    promise.future
  }

  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param targetVer 旧版本号
   * @return 新文档
   */
  def revert(targetVer: Int): Future[Option[T#Pack]] = {
    val promise = Promise[Option[T#Pack]]()
    val newDoc = specStorage.revert(targetVer)
    newDoc.onComplete {
      case Success(Some(d)) =>
        //snapDoc = Some(d)
        atomic {
          implicit txn =>
            latest_f.set(newDoc) //更新对最近文档的 STM 引用
            unFlushDoc.set(None)
        }
        promise.success(Some(d))
      case Success(None) => promise.success(None)
      case Failure(e) => promise.failure(e)
    }
    promise.future
  }

  /**
   * 恢复文档的指定版本，复制指定的旧版本新建一个新版本，版本号增加
   * @param targetTag 旧版本号
   * @return 新文档
   */
  def revert(targetTag: String): Future[Option[T#Pack]] = {
    val promise = Promise[Option[T#Pack]]()
    val newDoc = specStorage.revert(targetTag)
    newDoc.onComplete {
      case Success(Some(d)) =>
        //snapDoc = Some(d)
        atomic {
          implicit txn =>
            latest_f.set(newDoc) //更新对最近文档的 STM 引用
            unFlushDoc.set(None)
        }
        promise.success(Some(d))
      case Success(None) => promise.success(None)
      case Failure(e) => promise.failure(e)
    }
    promise.future
  }

  def latest = atomic {
    implicit txn => latest_f.get
  }

  def latestReload = load()

  def version(n: Int) = load(n)
}
