lazy val repro = project
  .in(file("repro"))
  .settings(
    scalaVersion := "3.3.7",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % "3.0.0-RC9",
      "dev.zio" %% "zio" % "2.1.9"
    )
  )
