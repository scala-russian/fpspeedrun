name := "fpspeedrun"

version := "mainrun-3"

scalaVersion := "2.12.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0"
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.12.0"
libraryDependencies += "com.github.julien-truffaut" %%  "newts-core"  % "0.3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)



