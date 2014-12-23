package zzb.config

import org.scalatest.WordSpec
import org.scalatest.MustMatchers

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-2
 * Time: 上午11:21
 * Copyright baoxian.com 2012~2020
 */
class EnvConfigTest extends WordSpec with MustMatchers with EnvConfigLoader {

  "env config" must {
    "override general config" in {
      System.setProperty("BOX_ENV","dev")
      val config = loadConfig("SomeConfig")
      config.get.getString("db.firstdb.user") must equal("dbuser")
      config.get.getInt("db.firstdb.pool.min") must equal(2)

      config.get.getInt("akka.remote.netty.tcp.port") must equal(2590)
      config.get.getString("akka.remote.netty.tcp.hostname") must equal("127.0.0.1")
    }

    "check config file no exist" in {
      val config = loadConfig("noThisFile")
      config must equal(None)
    }

    "work well when provided general config but no env config" in {
      val config = loadConfig("noEnvConfig")
      config.get.getString("hello.name") must equal("simon")
    }

    "general config has value,but env config empty block" in {

      val config = loadConfig("SomeConfig")

      config.get.getString("envIsEmpty.hello") must equal("simon")
    }
  }

}
