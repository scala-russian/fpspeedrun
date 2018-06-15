import scalariform.formatter.preferences._

name := "fpspeedrun"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

scalariformPreferences := scalariformPreferences.value
  .setPreference(DanglingCloseParenthesis, Force)
  .setPreference(AlignArguments, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DanglingCloseParenthesis, Force)
  .setPreference(FirstParameterOnNewline, Force)
