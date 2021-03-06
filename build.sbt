val dottyVersion = "0.21.0-RC1"

enablePlugins(JavaAppPackaging)
Compile / run / fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-new-syntax"
    ),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
