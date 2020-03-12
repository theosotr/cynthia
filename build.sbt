lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "gr.dmst.aueb.cynthia",
      scalaVersion := "2.13.1"
    )),
    name := "cynthia"
  )


libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.postgresql" % "postgresql" % "42.2.10",
  "mysql" % "mysql-connector-java" % "8.0.19",
  "org.xerial" % "sqlite-jdbc" % "3.30.1"
)
scalacOptions := Seq("-unchecked", "-deprecation")
