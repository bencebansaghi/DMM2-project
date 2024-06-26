ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "project"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaj" %% "scalaj-http" % "2.4.2",
      "com.typesafe.play" %% "play-json" % "2.10.0",
      "com.softwaremill.sttp.client3" %% "core" % "3.5.2",
      "com.softwaremill.sttp.client3" %% "circe" % "3.9.5"
))
