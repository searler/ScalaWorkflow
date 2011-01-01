/* Copyright (c) 2010 Richard Searle
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Richard Searle
 */
package cognitiveentity.workflow.akka

import cognitiveentity.workflow.{Bal,PP,Acct,Num}

import akka.camel.CamelContextManager
import akka.camel.CamelServiceManager._

object Responder{
   import cognitiveentity.workflow.CI
   import cognitiveentity.workflow.Services._
   def apply(a:Any) = {
     a match {
       case (ci:CI,id:Int)    => (ci,numMap(id))
       case (ci:CI,num:Num)   => (ci,acctMap(num))
       case (ci:CI,acct:Acct) => (ci,balMap(acct))
     }
   }
}


private class Service(name:String) extends akka.actor.Actor with akka.camel.Producer{
   def endpointUri = "seda:" + name
}
 
private object Service{
  val numService = akka.actor.Actor.actorOf(new Service("num")).start
  val acctService = akka.actor.Actor.actorOf(new Service("acct")).start
  val balService = akka.actor.Actor.actorOf(new Service("bal")).start
  val ppService = akka.actor.Actor.actorOf(new Service("pp")).start
}

private class Launcher extends AkkaFlowActor{  
 
  override def create(a:Any) =  cs(a)
  
  import Service._
  
  implicit val callNum = get[Int,List[Num]](numService)
  implicit val callAcct = get[Num,Acct](acctService)
  implicit val callBal = get[Acct,Bal](balService)
  implicit val callPP = get[Acct,PP](ppService)

  val cs = new cognitiveentity.workflow.FlowsSwitch


  override def receive = {
      case akka.camel.Message(a,_) => super.receive(a)
      case _ @ x => super.receive(x)
  }
}

private object Launcher {
   import cognitiveentity.workflow.Trigger
   def apply[A](initial:A) = {
     val actRef = akka.actor.Actor.actorOf[Launcher]
     actRef !! Trigger(initial)
   }
} 


object CamelTest extends org.specs.Specification {
   
     doBeforeSpec {
     CamelContextManager.init
     val context = CamelContextManager.mandatoryContext
     import org.apache.camel.scala.dsl.builder.RouteBuilder;
     context.addRoutes(new RouteBuilder{"seda:num".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:acct".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:bal".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:pp".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:request".bean(Launcher)})

     startCamelService
    }

   def send[A](a:A) = CamelContextManager.mandatoryContext.createProducerTemplate.requestBody("seda:request",a)

    "num" in  {
     Some(List(Num("124-555-1234"),Num("333-555-1234")))  must beEqualTo(send(123))
    }

    "bal" in  {
     Some(Bal(124.5F))  must beEqualTo(send(Num("124-555-1234")))
    }

   "manySingleThread" in {
     val template = CamelContextManager.mandatoryContext.createProducerTemplate
     for(i<-0 to 100)
       Some(Bal(124.5F))  must beEqualTo(template.requestBody("seda:request",Num("124-555-1234")))
   }


   "lots" in {
     import akka.actor.Actor._
     val template = CamelContextManager.mandatoryContext.createProducerTemplate
     val cnt  =16 //fails on greater
     var success = new java.util.concurrent.atomic.AtomicInteger
     val latch  = new java.util.concurrent.CountDownLatch(cnt)
     for(i<-0 until cnt)
       spawn{ 
           val result= template.requestBody("seda:request",Num("124-555-1234"));
           if(Some(Bal(124.5F)) == result)success.incrementAndGet
           latch countDown 
        }
   latch.await
   cnt must beEqualTo( success.get)
 
   }

    doAfterSpec {
      stopCamelService
      akka.actor.ActorRegistry.shutdownAll
    }
}


