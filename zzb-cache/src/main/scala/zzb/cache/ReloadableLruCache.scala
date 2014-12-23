package zzb.cache


import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import spray.caching.{SimpleLruCache, ExpiringLruCache}
import scala.util.{Failure, Success}

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-12
 * Time: 下午4:35
 * Copyright baoxian.com 2012~2020
 */


class ReloadableLruCache[K, V](val creator: K => Option[V],
                               maxCapacity: Int, initialCapacity: Int) {
  val inCache = new SimpleLruCache[V](maxCapacity, initialCapacity)

  def get(key: K): Option[Future[V]] = inCache.get(key)

  def take(key: K)(implicit ec: ExecutionContext): Future[Option[V]] = {

    val fv: Future[V] = inCache.get(key) match {
      case Some(f) => f
      case None => inCache.apply(key, () => Future {
        creator(key) match {
          case Some(v) => v
          case None => throw new NotFoundDataException(key)
        }
      })
    }
    val promise = Promise[Option[V]]()
    fv.onComplete {
      case Success(v) => promise.success(Some(v))
      case Failure(ex: NotFoundDataException) => promise.success(None)
      case Failure(ex) => promise.failure(ex)
    }
    promise.future
  }

  def remove(key: K): Option[Future[V]] = inCache.remove(key)

  def clear() = inCache.clear()

  def size = inCache.size

  def apply(key: K) = inCache.apply(key)
}

class NotFoundDataException(val key: Any) extends Exception

object ReloadableLruCache {
  def lruCache[K, V](creator: K => Option[V], maxCapacity: Int = 1024, initialCapacity: Int = 64,
                     timeToLive: Duration = Duration.Inf, timeToIdle: Duration = Duration.Inf) =
    new ReloadableLruCache[K, V](creator, maxCapacity, initialCapacity)
}