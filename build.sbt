name := "scala-signalr-client"
version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"

val akkaVersion = "2.3.9"
val akkaStreamsVersion = "1.0"
val akkaDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core-experimental" % akkaStreamsVersion,
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaStreamsVersion,
  "com.typesafe.akka" %% "akka-stream-experimental" % akkaStreamsVersion
)
libraryDependencies ++= akkaDependencies
