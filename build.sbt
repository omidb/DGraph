import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

val commonSettings = Seq(
  organization := "com.github.omidb",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8"),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.3.1" % "test"
  )
)

def preventPublication(p: Project) =
  p.settings(
    publish :=(),
    publishLocal :=(),
    publishSigned :=(),
    publishLocalSigned :=(),
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    packagedArtifacts := Map.empty)

lazy val dgraph = crossProject
  .settings(commonSettings: _*)
  .settings(
    name := "dgraph",
    scmInfo := Some(ScmInfo(
      url("https://github.com/omidb/dgraph"),
      "scm:git:git@github.com:omidb/dgraph.git",
      Some("scm:git:git@github.com:omidb/dgraph.git"))),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomExtra :=
      <url>https://github.com/omidb/dgraph</url>
        <licenses>
          <license>
            <name>MIT license</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
          </license>
        </licenses>
        <developers>
          <developer>
            <id>omidb</id>
            <name>Omid Bakhshandeh</name>
            <url>https://github.com/omidb</url>
          </developer>
        </developers>,
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  ).jsSettings(
  scalaJSStage in Global := FullOptStage
).jvmSettings(
)

lazy val dgraphJS = dgraph.js

lazy val dgraphJVM = dgraph.jvm

lazy val root = project.in(file(".")).
  settings(commonSettings: _*).
  settings(
    publish := {},
    publishLocal := {}
  ).
  aggregate(dgraphJS, dgraphJVM)
    