lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      version := "0.1",
      organization := "cynthia",
      scalaVersion := "2.13.4",
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision,
      scalacOptions := Seq("-Wunused:imports", "-unchecked", "-deprecation"),
      assemblyJarName in assembly := s"cynthia.jar"
    )
  ),
  name := "cynthia"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.slf4j" % "slf4j-simple" % "1.7.30",
  "com.github.scopt" %% "scopt" % "4.0.0",
  "org.postgresql" % "postgresql" % "42.2.10",
  "mysql" % "mysql-connector-java" % "8.0.19",
  "org.xerial" % "sqlite-jdbc" % "3.30.1",
  "org.scala-lang" % "scala-reflect" % "2.13.1",
  "com.lihaoyi" %% "pprint" % "0.5.9",
  "io.spray" %% "spray-json" % "1.3.5",
  "com.microsoft.sqlserver" % "mssql-jdbc" % "8.2.2.jre11",
  "me.tongfei" % "progressbar" % "0.9.0" from "file://" + file(
    "lib/progressbar-0.9.0.jar"
  ).getAbsolutePath,
  "com.microsoft.z3 " % "microsoft-z3" % "1.0" from "file://" + file(
    "lib/com.microsoft.z3.jar"
  ).getAbsolutePath
)
