package zzb.cache


import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration
import org.specs2.mutable.Specification

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-12
 * Time: 下午4:38
 * Copyright baoxian.com 2012~2020
 */
class CacheTest extends Specification {

  case class Person(name: String, age: Int)

  trait PersonLoader {
    def loadPerson(name: String): Option[Person]
  }

  class MapPersonLoader extends PersonLoader {

    val users = Map(
      "simon" -> Person("simon", 38),
      "jack" -> Person("jack", 20),
      "tom" -> Person("tom", 16)
    )

    def loadPerson(name: String): Option[Person] = {
      users.get(name)
    }
  }

  case class TheCaches(loader: PersonLoader) {

    import ReloadableLruCache._

    val cachePerson = lruCache[String, Person](loader.loadPerson)
  }

  "a ReloadableLruCache " should {

    import ExecutionContext.Implicits.global


    val theCaches = TheCaches(new MapPersonLoader)

    "can storage data" in {

      import spray.caching.ValueMagnet._


      theCaches.cachePerson.size === 0

      val p1 = Person("wolfgang", 7)
      val f0 = theCaches.cachePerson("wolfgang")(p1)

      val f1 = theCaches.cachePerson.get("wolfgang").get
      val r1 = Await.result(f1, Duration.Inf)

      theCaches.cachePerson.get("wolfgang").get
      r1 === p1
      //theCaches.cachePerson.size === 1

      val p2 = Person("wolfgang", 8)
      //theCaches.cachePerson.remove("wolfgang")
      theCaches.cachePerson("wolfgang").apply(p2)
      val f2 = theCaches.cachePerson.get("wolfgang").get
      Await.result(f2, Duration.Inf) === p1
      //theCaches.cachePerson.size === 1
    }

    "when no data in cache,can get from the loader" in {
      theCaches.cachePerson.get("simon") must be(None)

      val f1 = theCaches.cachePerson.take("simon")

      Await.result(f1, Duration.Inf) === Some(Person("simon", 38))
      //theCaches.cachePerson.size === 2

      val f2 = theCaches.cachePerson.take("vivian")

      Await.result(f2, Duration.Inf) === None

    }
  }


}
