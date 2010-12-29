import sbt._

class ScalaWorkflowProject(info: ProjectInfo) extends DefaultProject(info)  with AkkaProject
{
    val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6"

    val akkaRep = " Akka repository" at "http://www.scalablesolutions.se/akka/repository/"

    val akkaCamel = akkaModule("camel")
    val camelCore = "org.apache.camel" % "camel-core" % "2.5.0"
    val camelScala = "org.apache.camel" % "camel-scala" % "2.5.0"

}
