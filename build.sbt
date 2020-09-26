name := "typelevel-scala"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {
  Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
}