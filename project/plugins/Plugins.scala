import sbt._
 
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val akkaRep = " Akka repository" at "http://akka.io/repository"
  val akkaPlugin = "se.scalablesolutions.akka" % "akka-sbt-plugin" % "1.0"
  val scctRepo = "scct-repo" at "http://mtkopone.github.com/scct/maven-repo/"
  lazy val scctPlugin = "reaktor" % "sbt-scct-for-2.8" % "0.1-SNAPSHOT"

}
