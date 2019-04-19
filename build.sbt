name := "fix-symlinks"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalaz" %% "scalaz-zio" % "1.0-RC4"

wartremoverErrors ++= Warts.unsafe

wartremoverErrors -= Wart.Any