lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "gr.dmst.aueb.cynthia",
      scalaVersion := "2.13.1"
    )),
    name := "cynthia"
  )


libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
scalacOptions := Seq("-unchecked", "-deprecation")
