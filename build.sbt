// See LICENSE for license details.

name := "pfb"

organization := "edu.berkeley.cs"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "dsptools" -> "1.1-SNAPSHOT",
  "rocket-dsp-utils" -> "1.0",
  "chisel3" -> "3.1-SNAPSHOT",
  "chisel-iotesters" -> "1.2-SNAPSHOT",
  "tapeout" -> "0.1-SNAPSHOT"
  )

libraryDependencies ++= Seq("dsptools", "rocket-dsp-utils", "chisel3", "chisel-iotesters", "tapeout").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) }

libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"
libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "1.0.3" % "provided"
libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5",
  "org.scalacheck" %% "scalacheck" % "1.12.4")

//testOptions in Test += Tests.Argument("-oF")

lazy val TravisTest = config("travis") extend(Test)

lazy val pfb = (project in file(".")).
  configs(TravisTest).
  settings(inConfig(TravisTest)(Defaults.testTasks): _*)

testOptions in TravisTest += Tests.Argument(TestFrameworks.ScalaTest, "-l", "edu.berkeley.tags.LocalTest")

ghpages.settings

git.remoteRepo := "git@github.com:ucb-art/pfb.git"

site.includeScaladoc()
