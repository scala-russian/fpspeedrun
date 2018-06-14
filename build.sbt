name := "fpspeedrun"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "1.0.0-RC2",
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")