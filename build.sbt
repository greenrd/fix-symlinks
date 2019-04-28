name := "fix-symlinks"

version := "0.1"

organization := "org.greenrd"

scalaVersion := "2.12.8"

val derivingVersion = "1.0.0"

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-zio" % "1.0-RC4",
  "org.scalaz" %% "scalaz-core" % "7.2.25",
  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),

  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion,
  "com.nequissimus" %% "zio-slf4j" % "0.1.7+0-00bf62d1+20190422-1410-SNAPSHOT",
  "org.scala-lang" % "scala-reflect" % "2.12.8" % "runtime",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime"
)

wartremoverErrors in (Compile, compile) ++= Warts.unsafe

wartremoverErrors in (Compile, compile) --= Seq(Wart.Any, Wart.DefaultArguments)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

bintrayVcsUrl := Some("git@github.com:greenrd/fix-symlinks.git")

publishMavenStyle := false

bintrayRepository := "ivy"
