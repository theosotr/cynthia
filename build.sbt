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
  "org.xerial" % "sqlite-jdbc" % "3.30.1",
  "org.scala-lang" % "scala-reflect" % "2.13.1",
  "com.lihaoyi" %% "pprint" % "0.5.9",
  "io.spray" %%  "spray-json" % "1.3.5",
)
scalacOptions := Seq("-unchecked", "-deprecation")
