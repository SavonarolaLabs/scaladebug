ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "scaladebug",
    Compile / resourceDirectory := baseDirectory.value / "src" / "main" / "resources",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.5",
      "io.circe" %% "circe-generic" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5",
      "org.ergoplatform" %% "ergo-appkit" % "5.0.4" // for ErgoValue if needed
    ),
    dependencyOverrides ++= Seq(
      "io.circe" %% "circe-core" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5",
      "io.circe" %% "circe-generic" % "0.14.5"
    ),
    resolvers += "Ergo Platform Repository" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )
