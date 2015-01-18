import sbt._

object Globals {
  val name                  = "functional-scala"
  val scalaVersion          = "2.11.5"
  val crossScalaVersions    = Seq("2.11.5", "2.10.4")
  val jvmVersion            = "1.8"

  val homepage              = Some(url("http://www.pjan.io"))
  val startYear             = Some(2014)
  val summary               = "Functional Scala"
  val description           = "Functional shizzle in scala"
  val maintainer            = "pjan <pjan@pjan.io>"
  val license               = Some("MIT")

  val organizationName      = "pjan.io"
  val organization          = "io.pjan"
  val organizationHomepage  = Some(url("http://pjan.io"))

  val sourceUrl             = "https://github.com/pjan/functional-scala"
  val scmUrl                = "git@github.com:pjan/functional-scala.git"
  val scmConnection         = "scm:git:git@github.com:pjan/functional-scala.git"

  val serviceDaemonUser     = "admin"
  val serviceDaemonGroup    = "admin"

  val credentials: Seq[Credentials] = Seq[Credentials]()

  val snapshotRepo = Some("Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

  val dockerBaseImage       = "pjan/oracle-jdk-8"
  val dockerImageNamespace  = "pjan"

  val pomDevelopers = {
    <id>pjan</id> <name>pjan vandaele</name> <url>http://pjan.io</url>
  }

  val pomLicense = {
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/mit</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
  }

}
