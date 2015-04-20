import sbt._
import sbt.Keys._

object Dependencies {

  val resolutionRepos = Seq(
        "Spray Repository" at "http://repo.spray.io/",
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/releases/"

  )

  val excludeJars = ExclusionRule(organization = "com.typesafe.akka") ::
    ExclusionRule(organization = "com.typesafe.akka", name = "akka-actor") ::
    ExclusionRule(organization = "com.typesafe.akka", name = "akka-remote") ::
    ExclusionRule(organization = "com.typesafe.akka", name = "akka-slf4j") ::
    ExclusionRule(organization = "com.typesafe.akka", name = "akka-actor") ::
    ExclusionRule(organization = "log4j") ::
    ExclusionRule(organization = "org.slf4j", name = "slf4j-log4j12") ::
    ExclusionRule(organization = "org.slf4j", name = "slf4j-api") ::
    ExclusionRule(organization = "org.slf4j", name = "slf4j-nop") ::
    ExclusionRule(organization = "org.terracotta.toolkit") ::
    ExclusionRule(organization = "org.terracotta") ::
    ExclusionRule(organization = "net.sf.ehcache") ::
    Nil

  val akkaVersion = "2.3.9"
  val sprayVersion = "1.3.3"

  def compile(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "compile")

  def provided(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "provided")

  def test(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "test")

  def runtime(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "runtime")

  def container(deps: ModuleID*): Seq[ModuleID] = deps map (_ % "container")

  val scalaReflect = "org.scala-lang" % "scala-reflect" % "2.10.5"
  val quasiQuotes = "org.scalamacros" %% "quasiquotes" % "2.0.0" cross CrossVersion.binary
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaKernel = "com.typesafe.akka" %% "akka-kernel" % akkaVersion
  val akkaTestKit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion
  val akkaRemoteTest = "com.typesafe.akka" %% "akka-remote-tests" % akkaVersion
  val akkaRemote = "com.typesafe.akka" %% "akka-remote" % akkaVersion
  val sprayCan = "io.spray" %% "spray-can" % sprayVersion
  val sprayRouting = "io.spray" %% "spray-routing" % sprayVersion
  val sprayHttp = "io.spray" %% "spray-http" % sprayVersion
  val sprayJson = "io.spray" %% "spray-json" % "1.3.1"
  val sprayClient = "io.spray" %% "spray-client" % sprayVersion
  val sprayUtil = "io.spray" %% "spray-util" % sprayVersion
  val sprayCaching = "io.spray" %% "spray-caching" % sprayVersion
  val sprayTestkit = "io.spray" %% "spray-testkit" % sprayVersion
  val scalaloggingSlf4j = "com.typesafe" %% "scalalogging-slf4j" % "1.1.0" excludeAll(excludeJars: _*)
  val shapeless = "com.chuusai" %% "shapeless"    % "1.2.4"
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.0-RC1"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.2"
  val specs2 = "org.specs2" %% "specs2" % "2.2.3"

  val squeryl = "org.squeryl" %% "squeryl" % "0.9.6-RC2"
  val mysqlDriver = "mysql" % "mysql-connector-java" % "5.1.26"
  val casbah = "org.mongodb" %% "casbah" % "2.8.0"
  val c3p0 = "c3p0" % "c3p0" % "0.9.1.2"
  val bonecp= "com.jolbox" % "bonecp" % "0.8.0.RELEASE" excludeAll(excludeJars: _*)
  val sclick = "com.typesafe.slick" %% "slick-codegen" % "2.1.0"
  val sclickEx = "com.typesafe.slick" %% "slick-extensions" % "2.1.0"
  val h2 = "com.h2database" % "h2" % "1.2.127"

  val config = "com.typesafe" % "config" % "1.2.1"
  val nScalaTime = "com.github.nscala-time" %% "nscala-time" % "1.8.0"
  val scalaStm = "org.scala-stm" %% "scala-stm" % "0.7"

  val smackCore = "org.igniterealtime.smack" % "smack-core" % "4.0.2"
  val smackExts = "org.igniterealtime.smack" %  "smack-extensions" % "4.0.2"
  val smackTcp = "org.igniterealtime.smack" % "smack-tcp" % "4.0.2"
}
