name := """reactive-lab3"""

version := "1.1"

scalaVersion := "2.13.3"

val akkaVersion = "2.6.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"               % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit"             % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-persistence"         % akkaVersion,
  "org.scalatest"     %% "scalatest"                % "3.2.2" % "test",
  "ch.qos.logback"    % "logback-classic"           % "1.2.3"
)
