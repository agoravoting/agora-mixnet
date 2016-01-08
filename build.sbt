// scalaVersion := "2.12.0-M3"
scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.6.0",
  "com.chuusai" %% "shapeless" % "2.2.5"
)