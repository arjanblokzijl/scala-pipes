import sbt._
import Keys._


object ScalaPipesBuild extends Build {

  lazy val root = Project(
    id = "scala-pipes",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(pipes, examples)
  )

  lazy val pipes = Project(
    id = "pipes",
    base = file("pipes"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependencies.scalaz, Dependencies.scalazEffect, Dependencies.ScalaCheck, Dependencies.Specs)
    )
  )

  lazy val examples = Project(
    id = "pipes-examples",
    base = file("examples"),
    dependencies = Seq[ClasspathDep[ProjectReference]](pipes),
    settings = standardSettings
  )

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "ulysses",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.9.1",
//    scalaVersion := "2.10.0-M1",
    scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-Ydependent-method-types", "-Xlog-implicits"),
    resolvers += ScalaToolsSnapshots
  )

  object Dependencies {
    lazy val scalaz = "org.scalaz" % "scalaz-core_2.9.1" % "7.0-SNAPSHOT"
    lazy val scalazEffect = "org.scalaz" % "scalaz-effect_2.9.1" % "7.0-SNAPSHOT"
    //lazy val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test" withSources ()
    lazy val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"

    def ScalaCheck = "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"

    def Specs = "org.specs2" % "specs2_2.9.1" % "1.6.1" % "test"
  }
}
