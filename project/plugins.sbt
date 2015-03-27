resolvers += Classpaths.typesafeReleases

resolvers += "sbt-idea" at "http://mpeltonen.github.com/maven/"

resolvers += "Typesafe Repository" at "http://repo.akka.io/releases/"

resolvers += "Spray Repository" at "http://repo.spray.io/"

libraryDependencies ++= Seq(
  "com.decodified" % "scala-ssh" % "0.6.2",
  "org.bouncycastle" % "bcprov-jdk16" % "1.46",
  "com.jcraft" % "jzlib" % "1.1.1"
)

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.3")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-multi-jvm" % "0.3.8")

addSbtPlugin("io.spray" % "sbt-twirl" % "0.7.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.7")

resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
  url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")

//支持aws s3 发布的插件
addSbtPlugin("com.frugalmechanic" % "fm-sbt-s3-resolver" % "0.4.0")
