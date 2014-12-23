package zzb.config

import com.typesafe.config.{Config, ConfigResolveOptions, ConfigParseOptions, ConfigFactory}

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-1
 * Time: 上午3:44
 * Copyright baoxian.com 2012~2020
 */
trait EnvConfigLoader {

  def loadConfig(configName: String,withSysConfig :Boolean = true): Option[Config] = {

    //获取运行环境的名称
    //先从 JVM 启动参数中查找名为"BOX_ENV"的配置项目，
    //如果未找到就查找系统环境变量 "BOX_ENV"
    //如果 JVM 启动参数明确设置 ”BOX_ENV“为空串，就认为没有设置，不再查找系统参数
    val envName = scala.util.Properties.propOrElse("BOX_ENV",
      scala.util.Properties.envOrElse("BOX_ENV", ""))

    innerLoadConfig(configName, envName.trim,withSysConfig)
  }

  private def innerLoadConfig(configName: String, envName: String,withSysConfig :Boolean): Option[Config] = {
    val envConfigName =
      if (configName.endsWith(".conf"))
        s"env/$envName/$configName"
      else
        s"env/$envName/$configName.conf"

    //不允許配置文件不存在
    val parseOpt = ConfigParseOptions.defaults().setAllowMissing(false)

    try {
      //通用配置文件不允許不存在
      val generalConfig =
        if(withSysConfig)
        ConfigFactory.load(configName, parseOpt, ConfigResolveOptions.defaults)
      else ConfigFactory.parseResourcesAnySyntax(configName,parseOpt)

      if (envName.length > 0) {  //环境专有配置可以不存在
        val envConfig = ConfigFactory.parseResourcesAnySyntax(envConfigName,ConfigParseOptions.defaults)
        Some(envConfig.withFallback(generalConfig))
      }
      else
        Some(generalConfig)
    } catch {
      case ex: Throwable ⇒
        println(ex);None
    }
  }
}

object EnvConfigLoader extends EnvConfigLoader
