import sbt._
import Keys._


object Build extends Build {

  import BuildSettings._
  import Dependencies._


  // 让 sbt 命令行的提示符显示当前工程的名称
  override lazy val settings = super.settings :+ {
    shellPrompt := {
      s => Project.extract(s).currentProject.id + " > "
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Root Project   你可以把工程名称“root_project”换成你希望的名字
  // -------------------------------------------------------------------------------------------------------------------

  lazy val root = Project("root_project", file("."))
    .aggregate(service_a, service_b) //定义包含的子模块
    .dependsOn(service_a, service_b) //加上对所有子模块的依赖
    .settings(rootSetting: _*) //这一行一定要加上，这是sbt-box插件为root project定义的设置，打包时会有用到

  // -------------------------------------------------------------------------------------------------------------------
  // Modules
  // -------------------------------------------------------------------------------------------------------------------

  lazy val service_a = Project("service-a", file("service-a")) //Project 的第一个参数是工程名称，第二个参数是文件相对目录
    .settings(moduleSettings: _*)
    .settings(boxedServiceSettings: _*)
    .settings(libraryDependencies ++=
    compile(scalaloggingSlf4j, srvBox) ++
      test(scalatest) ++
      runtime(logback)
  )
  lazy val service_b = Project("service-b", file("service-b"))
    .dependsOn(service_a) //指定工程的依赖性
    .settings(moduleSettings: _*)
    .settings(boxedServiceSettings: _*)
    .settings(libraryDependencies ++=
    compile(scalaloggingSlf4j, srvBox) ++
      test(scalatest) ++
      runtime(logback)
  )
}