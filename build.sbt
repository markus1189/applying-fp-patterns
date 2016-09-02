name := "FP with cats"

scalaVersion := "2.10.6" // sad story :(

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "1.6.1" % "provided",
  "org.typelevel" %% "cats" % "0.7.0"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
