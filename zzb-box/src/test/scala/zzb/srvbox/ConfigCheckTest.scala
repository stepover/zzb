package zzb.srvbox

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import com.typesafe.config.ConfigException._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-4
 * Time: 下午4:53
 * Copyright baoxian.com 2012~2020
 */
class ConfigCheckTest extends WordSpec with MustMatchers {

  "BoxApp config" must {
    "throw exception if not found 'services' config " in {
      intercept[Missing] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/noServices"))
      }
    }
    "throw exception if not found 'services.names' config " in {
      intercept[Missing] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/noNames"))
      }
    }

    "throw exception if 'services.names' is not List " in {
      intercept[WrongType] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/namesNotList"))
      }
    }

    "throw exception if no 'serviceClass' for service " in {
      intercept[Missing] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/noServiceClass"))
      }
    }

    "use Specified file in cmd replace the default 'srvbox.conf' " in {
      val (config, _) = BoxBuilder.getSelectService(List("testConfigs/spec"))
      config.getConfig("myapp").getString("name") must equal("bar")
    }

    "use defaut value if some field not provide" in {
      val (_, services) = BoxBuilder.getSelectService(List("testConfigs/spec"))
      services(0).initStart must equal(1)
      services(0).shareSystem must equal(1)
      services(0).startTimeout mustBe 60 //缺省值
      services(1).initStart must equal(0)
      services(1).shareSystem must equal(0)
      services(1).dependOn(0) mustBe "S1"
      services(1).startTimeout mustBe 120 //配置文件中有指定
    }
    "use default empty config if not found sub services config file  " in {
      val (config, _) = BoxBuilder.getSelectService(List("testConfigs/noSubServiceConfigFile"))
      config.hasPath("services.S3") mustBe true
    }
    "throw exception if Specified not exist config file   " in {
      intercept[NotResolved] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/noThisFile"))
      }
    }
    "throw exception if sub service class not found" in {
      intercept[ClassNotFoundException] {
        val (_, _) = BoxBuilder.getSelectService(List("testConfigs/noExistSubServiceClassName"))
      }
    }

    "use srvbox.conf if not provide config from cmd " in {
      val (config, services) = BoxBuilder.getSelectService(Nil)
      config.getConfig("myapp").getString("name") must equal("foo")
      services.length must equal(2)
    }

    "if select sub service from cmd,only select services will be exec" in {
      val (_, services) = BoxBuilder.getSelectService(List("srvbox", "S1"))
      services.length must equal(1)
      services.head.name must equal("S1")
    }

    "服务独立配置文件中的信息会插入到主配置，并覆盖主配置中对应服务的信息" in {
      val (config, _) = BoxBuilder.getSelectService(List("srvbox", "S1"))
      val s1Config = config.getConfig("services.S1")
      s1Config.getString("tag") mustBe "in sub config file"

    }
  }

}
