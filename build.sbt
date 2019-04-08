import NativePackagerHelper._
name := """gobsec-web"""

version := "2.0.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)
//lazy val languageservice = project

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  cache,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.0-RC1" % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.url("Typesafe Ivy releases", url("https://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)

mappings in Universal ++= directory("Syntax")
mappings in Universal ++= directory("Examples")

