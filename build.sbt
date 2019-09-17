import Dependencies._

resolvers += Resolver.sonatypeRepo("releases")

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-deprecation:false",
  "-Xcheckinit",
  "-Xlint:-nullary-unit",
  "-Ymacro-annotations",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

lazy val root = (project in file("."))
  .settings(
    name := "fsis",
    libraryDependencies ++= Seq(
      simulacrum,
      scalaCheck % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
