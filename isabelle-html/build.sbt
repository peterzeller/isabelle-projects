import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
      name := "isabelle-html",
      resolvers += "jitpack" at "https://jitpack.io",
      libraryDependencies += scalaTest % Test,
      libraryDependencies += "org.tukaani" % "xz" % "1.8",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      libraryDependencies += "com.github.peterzeller" % "scala-prettyprint" % "ee7edfecf7"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
