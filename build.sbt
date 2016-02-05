// scalaVersion := "2.12.0-M3"
scalaVersion := "2.11.7"

lazy val akkaVersion = "2.4.0"
// fork in run := true

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.6.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.consensusresearch" %% "scrypto" % "1.0.4",
  "org.apache.commons" % "commons-collections4" % "4.0"
)

assemblyMergeStrategy in assembly := {
  case PathList("ch", "bfh", xs @ _*) => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

scalacOptions ++= Seq("-feature", "-language:existentials")
// javacOptions ++= Seq("-Xlint:unchecked")