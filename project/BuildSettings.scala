import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import java.lang.Boolean.getBoolean
import sbt._
import Keys._
import org.sbtidea.SbtIdeaPlugin._
import com.typesafe.sbt.{SbtScalariform, SbtMultiJvm}
import com.typesafe.sbt.SbtMultiJvm.MultiJvmKeys._

import bintray.Plugin._
import bintray.Keys._

object BuildSettings {
  val VERSION = "0.1.2-RC6"

  lazy val basicSettings = seq(
    version := NightlyBuildSupport.buildVersion(VERSION),
    homepage := Some(new URL("http://zzb.stepover.me")),
    organization := "me.stepover",
    organizationHomepage := Some(new URL("http://zzb.stepover.me")),
    description := "Restful framework base on Scala,Akka,Spary",
    startYear := Some(2013),
    licenses +=("MIT", url("http://opensource.org/licenses/MIT")),
    scalaVersion := "2.10.5",
    publishMavenStyle := true,
    resolvers ++= Dependencies.resolutionRepos,
    scalacOptions := Seq(
      "-encoding", "utf8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-target:jvm-1.6",
      "-language:_",
      "-Xlog-reflective-calls",
      "-language:reflectiveCalls",
      "-language:existentials",
      "-language:experimental.macros"
    ),
    ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
    libraryDependencies += "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary

  )

//  lazy val releaseSetting = seq(
//    repository in bintray := "release"
//  )

  lazy val releaseSetting = seq(
    // publishing
    crossPaths := true,
    publishTo :=
      Some("zzb snapshot" at {
        // public uri is repo.spray.io, we use an SSH tunnel to the nexus here
        "s3://s3.cn-north-1.amazonaws.com.cn/aws.baoxian.com/release/"
      })
  )

  lazy val snapshotSetting = seq(
    // publishing
    crossPaths := true,
    publishTo :=
      Some("zzb snapshot" at {
        // public uri is repo.spray.io, we use an SSH tunnel to the nexus here
        "s3://s3.cn-north-1.amazonaws.com.cn/aws.baoxian.com/snapshot/"
      })
  )

  lazy val publishSetting = if (VERSION.trim.endsWith("SNAPSHOT")) snapshotSetting else releaseSetting

  lazy val boxedServiceSettings =
    seq(
      mainClass in(Compile, run) := Some("zzb.srvbox.BoxApp")
    )

  lazy val zzbBoxedServiceSettings = zzbModuleSettings ++ boxedServiceSettings

  lazy val defaultMultiJvmScalatestOptions: Seq[String] = {
    val excludeTags = useExcludeTestTags.toSeq
    Seq("-C", "org.scalatest.akka.QuietReporter") ++
      (if (excludeTags.isEmpty) Seq.empty else Seq("-l", if (multiNodeEnabled) excludeTags.mkString("\"", " ", "\"") else excludeTags.mkString(" "))) ++
      (if (useOnlyTestTags.isEmpty) Seq.empty else Seq("-n", if (multiNodeEnabled) useOnlyTestTags.mkString("\"", " ", "\"") else useOnlyTestTags.mkString(" ")))
  }

  lazy val zzbModuleMultiJvmSettings = multiJvmSettings ++ formatSettings ++ zzbModuleSettings ++ Seq(
    parallelExecution in Test := false,
    extraOptions in MultiJvm <<= (sourceDirectory in MultiJvm) { src =>
      (name: String) => (src ** (name + ".conf")).get.headOption.map("-Dakka.config=" + _.absolutePath).toSeq
    },
    scalatestOptions in MultiJvm := defaultMultiJvmScalatestOptions
  )

  lazy val disableParallelTestSetting = seq(parallelExecution in Test := false)

  lazy val bintray_user = scala.util.Properties.propOrElse("bintray_user",
    scala.util.Properties.envOrElse("bintray_user", ""))

  lazy val zzbModuleSettingsBase =
    basicSettings ++ publishSetting ++
      NightlyBuildSupport.settings ++
      net.virtualvoid.sbt.graph.Plugin.graphSettings ++
      seq(
        // scaladoc settings
        (scalacOptions in doc) <++= (name, version).map {
          (n, v) => Seq("-doc-title", n, "-doc-version", v)
        }
      )

  lazy val zzbModuleSettings =  zzbModuleSettingsBase

  lazy val noPublishing = seq(
    publish :=(),
    publishLocal :=()
  )

  lazy val defaultMultiJvmOptions: Seq[String] = {
    import scala.collection.JavaConverters._

    val MultinodeJvmArgs = "multinode\\.(D|X)(.*)".r
    val knownPrefix = Set("multnode.", "akka.", "MultiJvm.")
    val akkaProperties = System.getProperties.propertyNames.asScala.toList.collect {
      case MultinodeJvmArgs(a, b) =>
        val value = System.getProperty("multinode." + a + b)
        "-" + a + b + (if (value == "") "" else "=" + value)
      case key: String if knownPrefix.exists(pre => key.startsWith(pre)) => "-D" + key + "=" + System.getProperty(key)
    }

    "-Xmx256m" :: akkaProperties :::
      (if (getBoolean("sbt.log.noformat")) List("-Dakka.test.nocolor=true") else Nil)
  }

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test := formattingPreferences
  )

  def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
  }

  def systemPropertyAsSeq(name: String): Seq[String] = {
    val prop = System.getProperty(name, "")
    if (prop.isEmpty) Seq.empty else prop.split(",").toSeq
  }

  // for excluding tests by tag use system property: -Dakka.test.tags.exclude=<tag name>
  // note that it will not be used if you specify -Dakka.test.tags.only
  lazy val useExcludeTestTags: Set[String] = {
    if (useOnlyTestTags.isEmpty) systemPropertyAsSeq("akka.test.tags.exclude").toSet
    else Set.empty
  }

  // for running only tests by tag use system property: -Dakka.test.tags.only=<tag name>
  lazy val useOnlyTestTags: Set[String] = systemPropertyAsSeq("akka.test.tags.only").toSet

  def executeMultiJvmTests: Boolean = {
    useOnlyTestTags.contains("long-running") || !useExcludeTestTags.contains("long-running")
  }

  val multiNodeEnabled = getBoolean("akka.test.multi-node")

  lazy val multiJvmSettings = SbtMultiJvm.multiJvmSettings ++ inConfig(MultiJvm)(SbtScalariform.scalariformSettings) ++ Seq(
    jvmOptions in MultiJvm := defaultMultiJvmOptions,
    //compileInputs in MultiJvm <<= (compileInputs in MultiJvm) dependsOn (ScalariformKeys.format in MultiJvm),
    compile in MultiJvm <<= (compile in MultiJvm) triggeredBy (compile in Test),
    //unmanagedSourceDirectories in Test <+= baseDirectory { _ / "src" / "multi-jvm" / "scala" },
    ScalariformKeys.preferences in MultiJvm := formattingPreferences) ++
    Option(System.getProperty("akka.test.multi-node.hostsFileName")).map(x => Seq(multiNodeHostsFileName in MultiJvm := x)).getOrElse(Seq.empty) ++
    Option(System.getProperty("akka.test.multi-node.java")).map(x => Seq(multiNodeJavaName in MultiJvm := x)).getOrElse(Seq.empty) ++
    Option(System.getProperty("akka.test.multi-node.targetDirName")).map(x => Seq(multiNodeTargetDirName in MultiJvm := x)).getOrElse(Seq.empty) ++
    ((executeMultiJvmTests, multiNodeEnabled) match {
      case (true, true) =>
        executeTests in Test <<= (executeTests in Test, multiNodeExecuteTests in MultiJvm) map {
          case (testResults, multiNodeResults) =>
            val overall =
              if (testResults.overall.id < multiNodeResults.overall.id)
                multiNodeResults.overall
              else
                testResults.overall
            Tests.Output(overall,
              testResults.events ++ multiNodeResults.events,
              testResults.summaries ++ multiNodeResults.summaries)
        }
      case (true, false) =>
        executeTests in Test <<= (executeTests in Test, executeTests in MultiJvm) map {
          case (testResults, multiNodeResults) =>
            val overall =
              if (testResults.overall.id < multiNodeResults.overall.id)
                multiNodeResults.overall
              else
                testResults.overall
            Tests.Output(overall,
              testResults.events ++ multiNodeResults.events,
              testResults.summaries ++ multiNodeResults.summaries)
        }
      case (false, _) => Seq.empty
    })

  lazy val docsSettings = basicSettings ++ noPublishing ++ seq(
    unmanagedSourceDirectories in Test <<= baseDirectory {
      _ ** "code" get
    }
  )

  lazy val exampleSettings = basicSettings ++ noPublishing

  lazy val exampleBoxedServiceSettings = exampleSettings ++ boxedServiceSettings ++
    seq(parallelExecution in Test := false)

}