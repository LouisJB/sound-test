val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sound-test-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
