val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sound-test-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:existentials",
      "-Werror",
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
