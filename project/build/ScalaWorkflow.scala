import sbt._

import reaktor.scct.ScctProject


class ScalaWorkflowProject(info: ProjectInfo) extends DefaultProject(info)  with AkkaProject with ScctProject
{
    val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7"

   
    val akkaCamel = akkaModule("camel")
    val camelCore = "org.apache.camel" % "camel-core" % "2.6.0"
    val camelScala = "org.apache.camel" % "camel-scala" % "2.6.0"

}
