name := "fix-symlinks"

version := "0.1"

scalaVersion := "2.12.8"

val derivingVersion = "1.0.0"

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-zio" % "1.0-RC4",
  "org.scalaz" %% "scalaz-core" % "7.2.25",
  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),

  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion
)

wartremoverErrors ++= Warts.unsafe

wartremoverErrors --= Seq(Wart.Any, Wart.DefaultArguments)
