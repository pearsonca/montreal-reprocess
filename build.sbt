import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

scalaVersion := "2.11.6"

sbtVersion := "0.13.7"

scalacOptions ++= Seq("-feature","-deprecation","-target:jvm-1.8")

name := "montreal-process"

version := "0.2"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.3" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
)
