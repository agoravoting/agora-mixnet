// scalaVersion := "2.12.0-M3"
scalaVersion := "2.11.7"

lazy val akkaVersion = "2.4.0"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.6.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion
)

assemblyMergeStrategy in assembly := {
  case PathList("ch", "bfh", xs @ _*) => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}