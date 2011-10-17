name := "Scala Workflow"

version := "0.1"

scalaVersion := "2.9.1"

sourceDirectory := file("src")

//Specs2
libraryDependencies ++= Seq(
   "org.specs2" %% "specs2" % "1.6.1",
   "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"
   )

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
   "releases"  at "http://scala-tools.org/repo-releases")

//AKKA

libraryDependencies ++= Seq(
   "se.scalablesolutions.akka" % "akka-actor" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-sbt-plugin" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-amqp" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-typed-actor" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-camel" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-remote" % "1.2"
   ,"se.scalablesolutions.akka" % "akka-camel-typed" % "1.2"
   ,"org.apache.camel" % "camel-scala" % "2.6.0"
 )

resolvers ++= Seq("Akka Repository" at "http://akka.io/repository")

resolvers ++= Seq("Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases")

parallelExecution in Test := false

