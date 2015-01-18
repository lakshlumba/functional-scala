import sbt._
import sbt.Keys._


object Projects extends Build {
  import play.twirl.sbt.SbtTwirl
  import Settings._
  import Unidoc.{settings => unidocSettings}
  import Assembly.{settings => assemblySettings}
  import Package.{serverSettings => packageServerSettings}
  import Release.{settings => releaseSettings}
  import Docker.{settings => dockerSettings}
  import AspectJ.{settings => aspectJSettings}
  import ScalaJs.{settings => scalaJsSettings}
  import Dependencies._

  lazy val root = Project(id = Globals.name, base = file("."))
    .settings(basicSettings: _*)
    .settings(noPublishing: _*)
    .aggregate(
      coreModule,
      fpinscalaModule,
      scalazModule
    )

  lazy val coreModule = module("core", basicSettings)
    .settings(
      libraryDependencies ++=
        compile() ++
        test(scalaTest, scalaCheck, scalaMock)
    )

  lazy val fpinscalaModule = module("fpinscala", basicSettings)
    .settings(
      libraryDependencies ++=
        compile() ++
        test(scalaTest, scalaCheck, scalaMock)
    )

  lazy val scalazModule = module("scalaz", basicSettings)
    .settings(
      libraryDependencies ++=
        compile(scalaz) ++
        test(scalaTest, scalaCheck, scalaMock)
    )

  def module(name: String, basicSettings: Seq[Setting[_]]): Project = {
    val id = s"${Globals.name}-$name"
    Project(id = id, base = file(id), settings = basicSettings ++ Seq(Keys.name := id))
  }

  val noPublishing: Seq[Setting[_]] = Seq(publish := { }, publishLocal := { }, publishArtifact := false)

}
