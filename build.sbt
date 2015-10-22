lazy val root = (project in file(".")).
  settings(
    name := "clp",
    version := "0.1",
    scalaVersion := "2.11.7"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")
