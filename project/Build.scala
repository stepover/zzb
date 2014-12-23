import sbt._
import Keys._
import com.typesafe.sbt.SbtMultiJvm.MultiJvmKeys._
import twirl.sbt.TwirlPlugin.Twirl
import twirl.sbt.TwirlPlugin._

object Build extends Build {

  import BuildSettings._
  import Dependencies._

  // configure prompt to show current project
  override lazy val settings = super.settings :+ {
    shellPrompt := {
      s => Project.extract(s).currentProject.id + " > "
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Root Project
  // -------------------------------------------------------------------------------------------------------------------

  lazy val root = Project("srv-utils", file("."))
    .aggregate(zzbBox, envConfig, dbAccess, zzbShell, zzbDatatype, zzbStorage, zzbRest, zzbRestTests, zzbRestTestKit, zzbDomain,zzbUtil, zzbXmpp,examples)
    .settings(basicSettings: _*)
    .settings(noPublishing: _*)


  // -------------------------------------------------------------------------------------------------------------------
  // Modules
  // -------------------------------------------------------------------------------------------------------------------

  lazy val zzbBox = Project("zzb-box", file("zzb-box"))
    .dependsOn(envConfig, dbAccess, zzbShell, zzbRest,zzbUtil)
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(akkaActor, akkaRemote, sprayCan, sprayRouting, sprayJson, scalaloggingSlf4j) ++
      test(scalatest, h2, akkaTestKit, mysqlDriver) ++
      runtime(akkaSlf4j, logback)
    )


  lazy val envConfig = Project("zzb-config", file("zzb-config"))
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(config) ++
      test(scalatest) ++
      runtime(akkaSlf4j, logback)
    )

  lazy val dbAccess = Project("zzb-dbaccess", file("zzb-dbaccess"))
    .dependsOn(zzbDatatype)
    .settings(zzbModuleSettings: _*)
    .settings(disableParallelTestSetting: _*)
    .settings(libraryDependencies ++=
    compile(config, c3p0, squeryl, scalaloggingSlf4j,casbah,bonecp,sclick,sclickEx) ++
      test(scalatest, h2) ++
      runtime(akkaSlf4j, logback)
    )
  lazy val zzbShell = Project("zzb-shell", file("zzb-shell"))
    .dependsOn(envConfig,zzbUtil)
    .settings(zzbModuleSettings: _*)
    .settings(disableParallelTestSetting: _*)
    .settings(libraryDependencies ++=
    compile(scalaloggingSlf4j, logback, nScalaTime, config, akkaActor, akkaRemote, sprayCaching) ++
      test(scalatest, akkaTestKit) ++
      runtime(akkaSlf4j, logback)
    )

  lazy val zzbDatatype = Project("zzb-datatype", file("zzb-datatype"))
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(nScalaTime, scalaReflect, sprayJson, quasiQuotes) ++
      test(scalatest, specs2, sprayRouting, sprayTestkit) ++
      runtime(logback)
    )
  lazy val zzbRest = Project("zzb-rest", file("zzb-rest"))
    .dependsOn(zzbDatatype,zzbUtil)
    .settings(zzbModuleMultiJvmSettings: _*)
    .settings(libraryDependencies ++=
    compile(nScalaTime, sprayRouting, akkaActor, akkaRemote, sprayJson, sprayCaching,scalaReflect, quasiQuotes) ++
      test(scalatest, specs2, akkaTestKit, sprayTestkit, akkaRemoteTest) ++
      runtime(akkaSlf4j,logback)
    ) configs MultiJvm

  lazy val zzbStorage = Project("zzb-storage", file("zzb-storage"))
    .dependsOn(zzbDatatype,zzbUtil,dbAccess)
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(casbah, sprayCaching, akkaActor, scalaStm) ++
      test(scalatest) ++
      runtime(logback)
    )

  lazy val zzbRestTestKit = Project("zzb-rest-testkit", file("zzb-rest-testkit"))
    .dependsOn(zzbRest,zzbUtil)
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(akkaTestKit, akkaActor, scalatest, specs2, shapeless))
  //
  //    //.settings(libraryDependencies ++= akkaTestKit +: provided(akkaActor, scalatest, specs2))

  lazy val zzbRestTests = Project("zzb-rest-tests", file("zzb-rest-tests"))
    .dependsOn(zzbRest, zzbRestTestKit,zzbUtil)
    .settings(zzbModuleSettings: _*)
    .settings(noPublishing: _*)
    .settings(libraryDependencies ++= test(sprayJson, specs2, akkaActor))

  lazy val zzbDomain = Project("zzb-domain", file("zzb-domain"))
    .dependsOn(zzbRest, zzbDatatype, zzbStorage,zzbUtil)
    .settings(zzbModuleSettings: _*)
    .settings(Twirl.settings: _*)
    .settings(libraryDependencies ++=
    compile(akkaActor, akkaRemote, scalaloggingSlf4j, nScalaTime) ++
      test(scalatest, akkaTestKit, sprayTestkit) ++
      runtime(akkaSlf4j, logback)
    )
  lazy val zzbUtil = Project("zzb-util", file("zzb-util"))
    .settings(zzbModuleSettings: _*)
    .settings(libraryDependencies ++=
    compile(  scalaloggingSlf4j,logback,nScalaTime,sprayUtil) ++
      provided(akkaActor, scalaReflect) ++
      test(scalatest) ++
      runtime(akkaSlf4j, logback)
    )
  lazy val zzbXmpp = Project("zzb-xmpp", file("zzb-xmpp"))
    .settings(zzbModuleSettings: _*)
    .settings(disableParallelTestSetting:_*)
    .settings(libraryDependencies ++=
    compile(scalaloggingSlf4j,nScalaTime,smackTcp,akkaActor,smackCore,smackExts,sprayUtil) ++
      test(scalatest,akkaTestKit) ++
      runtime(akkaSlf4j, logback)
    )
  // -------------------------------------------------------------------------------------------------------------------
  // Example Projects
  // -------------------------------------------------------------------------------------------------------------------

  lazy val examples = Project("examples", file("examples"))
    .aggregate(srvBoxDemoServiceExamples)
    .settings(exampleSettings: _*)

  lazy val srvBoxDemoServiceExamples = Project("srvbox-demoService", file("examples/srvbox-demoService"))
    .dependsOn(zzbBox, zzbRest)
    .settings(exampleBoxedServiceSettings: _*)
    .settings(libraryDependencies ++=
    compile(akkaActor, akkaRemote, sprayCan, sprayRouting, sprayJson, sprayClient, squeryl, c3p0, scalaloggingSlf4j) ++
      test(scalatest, akkaTestKit, specs2, h2) ++
      runtime(akkaSlf4j, mysqlDriver, logback, h2)
    )

  lazy val demoShell = Project("demo-shell", file("examples/demo-shell"))
    .dependsOn(zzbShell)
    .settings(exampleBoxedServiceSettings: _*)


}