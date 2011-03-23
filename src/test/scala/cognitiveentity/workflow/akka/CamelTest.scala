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
 * Simulate a real world deployment where services are remoted and accessed via
 * Camel (e.g. over JMS)
 *
 * Both Request/Response and fire-and-forget(oneway) interactions are tested.
 * In the latter case, the result is delivered to a fixed end point (e.g. a JMS
 * queue)
 *
 * @author Richard Searle
 */
package cognitiveentity.workflow.akka

import cognitiveentity.workflow.{Bal,PP,Acct,Num}

import akka.camel.CamelContextManager
import akka.camel.CamelServiceManager._

/**
 * Simulates external services that provides
 *  id             -> corresponding phone numbers
 *  phone number   -> account number
 *  account number -> balance
 *
 */
private object Responder{
   import cognitiveentity.workflow.CI
   import cognitiveentity.workflow.ValueMaps._

   def apply(a:Any) = {
     a match {
       case (ci:CI,id:Int)    => (ci,numMap(id))
       case (ci:CI,num:Num)   => (ci,acctMap(num))
       case (ci:CI,acct:Acct) => (ci,balMap(acct))
     }
   }
}


 /**
  * Implement lookup services using a seda end points
  */
private object Service{
  private class Service(name:String) extends akka.actor.Actor with akka.camel.Producer{
     def endpointUri = "seda:" + name
  }

  val numService = akka.actor.Actor.actorOf(new Service("num")).start
  val acctService = akka.actor.Actor.actorOf(new Service("acct")).start
  val balService = akka.actor.Actor.actorOf(new Service("bal")).start
  val ppService = akka.actor.Actor.actorOf(new Service("pp")).start
}

/**
 * Assemble:
 * - Camel
 * - FlowsSwitch
 * - above services
 */
private trait Wiring extends AkkaFlowActor {  
 
  //Return the flow that handles a
  override def create(a:Any) =  cs(a)
  
  import Service._
  
  implicit val callNum = get[Int,List[Num]](numService)
  implicit val callAcct = get[Num,Acct](acctService)
  implicit val callBal = get[Acct,Bal](balService)
  implicit val callPP = get[Acct,PP](ppService)

  val cs = new cognitiveentity.workflow.FlowsSwitch

  // unwrap contents of Camel Message
  override def receive = {
      case akka.camel.Message(a,_) => super.receive(a)
      case _ @ x => super.receive(x)
  }
}

/**
 * Perform R/R MEP against flow defined in Wiring
 */
private class RRLauncher extends RequestResponseAkkaFlowActor with Wiring
private object RRLauncher {
   import cognitiveentity.workflow.Trigger
   def apply[A](initial:A) = {
     val actRef = akka.actor.Actor.actorOf[RRLauncher] //nust occur here
     actRef !! Trigger(initial)
   }
} 

/**
 * Implements a Camel end point to asynchronously catch the results
 * of a oneway interaction
 */
private object RespondTo {
  import akka.actor.Actor
  import akka.actor.Actor._
  import akka.camel.Producer
 
  private class RespondTo extends Actor with Producer {
      def endpointUri = "seda:gather"
  }

   val dest = actorOf[RespondTo].start
}

/**
 * Perform oneway MEP against flow defined in Wiring,
 * with ultimate result delivered asynchronously to RespondTo
 */
private class SendLauncher(respondTo:akka.actor.ActorRef) extends FixedResponseAkkaFlowActor(respondTo) with Wiring
private object SendLauncher {
   import cognitiveentity.workflow.Trigger
   def apply[A](initial:A)  {
     val actRef = akka.actor.Actor.actorOf(new SendLauncher(RespondTo.dest)) //must occur here
     actRef ! Trigger(initial)
   }
} 

/**
 * Mechanism to asynchronously capture the results of a one-way
 * interaction.
 */
private object Gather {
    import java.util.concurrent._
    import java.util.concurrent.atomic._
    import scala.collection.mutable._
    val awaiter = new AtomicReference[CountDownLatch]
    val values = new ListBuffer[Any]

    //Reset the state and indicate the number of expected results
    def prep(expected:Int){
        synchronized {
           values.clear
           awaiter.set(new CountDownLatch(expected))
         }
    }
    //wait for expected number of results to be received
    def await{awaiter.get.await(2,java.util.concurrent.TimeUnit.SECONDS)}

    //record the result
    def apply[A](arg:A) {
        synchronized {
          values+=arg
          awaiter.get.countDown
       }
     }

   def get = synchronized {values toList}   
}

object CamelTest extends org.specs.Specification {
   
   doBeforeSpec {
     CamelContextManager.init
     val context = CamelContextManager.mandatoryContext

     import org.apache.camel.scala.dsl.builder.RouteBuilder;
     //wire end points for services to a single bean
     context.addRoutes(new RouteBuilder{"seda:num".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:acct".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:bal".bean(Responder)}) 
     context.addRoutes(new RouteBuilder{"seda:pp".bean(Responder)}) 
     //wire end point for R-R call of flow
     context.addRoutes(new RouteBuilder{"seda:request".bean(RRLauncher)})
     //wire end point for fire and forget call of flow
     context.addRoutes(new RouteBuilder{"seda:send".bean(SendLauncher)})
     //wire end point for result of f-a-f call 
     context.addRoutes(new RouteBuilder{"seda:gather".bean(Gather)})

     startCamelService
   }

   /**
    * Perform R/R MEP via Camel seda
   */
   def request[A](a:A) = CamelContextManager.mandatoryContext.createProducerTemplate.requestBody("seda:request",a)
   /**
    * Perform a f-a-f MEP via Camel seda
   */
   def send[A](a:A) = CamelContextManager.mandatoryContext.createProducerTemplate.sendBody("seda:send",a)

    /**
    * Multiple concurrent fire-and-forget invocations to return total balance
    * for an id.
    */
   "sumBalancesSendLots" in  {
     val cnt = 200
     Gather.prep(cnt)
     for(i<-0 until cnt)
         akka.actor.Actor.spawn{send(cognitiveentity.workflow.SumBalances(123))}
     Gather.await
     cnt must beEqualTo(Gather.get.length) 
     Gather.get.foreach { Bal(125.5F) must beEqualTo(_) }
    }
    
    /**
     * A single concurrent fire-and-forget invocation
     */
   "numSendOne" in  {
     Gather.prep(1)
     akka.actor.Actor.spawn{send(123)}
     Gather.await
     List(List(Num("124-555-1234"), Num("333-555-1234"))) must beEqualTo(Gather.get) 
    }

   /**
    * Multiple concurrent fire-and-forget invocations to return phone numbers
    * for an id.
    */
   "numSendLots" in  {
     val cnt = 200
     Gather.prep(cnt)
     for(i<-0 until cnt)
         akka.actor.Actor.spawn{send(123)}
     Gather.await
     cnt must beEqualTo(Gather.get.length) 
     Gather.get.foreach { List(Num("124-555-1234"), Num("333-555-1234")) must beEqualTo(_)    }
    }

    /**
     * Determine phone numbers from id
     */
    "num" in  {
     Some(List(Num("124-555-1234"),Num("333-555-1234")))  must beEqualTo(request(123))
    }

    /**
     * Determine balance from phone number
     */
    "bal" in  {
     Some(Bal(124.5F))  must beEqualTo(request(Num("124-555-1234")))
    }

   /**
    * Triggers  concurrent lookups from a single flow
    */
   "SumBalances" in  {
     Some(Bal(125.5F))  must beEqualTo(request(cognitiveentity.workflow.SumBalances(123)))
    }

    /**
     * Perform many serialized request-response interactions.
     */
   "manySingleThread" in {
     val template = CamelContextManager.mandatoryContext.createProducerTemplate
     for(i<-0 to 100)
       Some(Bal(124.5F))  must beEqualTo(template.requestBody("seda:request",Num("124-555-1234")))
   }

    /**
     * Perform several concurrent request/response invocations of the flows.
     * The number is limited by the size of thread pool that contains
     * the anonymous actors that perform the MEP.
     * This test simulates a reference from a blocking client,
     * perhaps a request-response web service.
     */
   "lots" in {
     import akka.actor.Actor._
     val template = CamelContextManager.mandatoryContext.createProducerTemplate
     val cnt  = 15 //fails on greater than pool size
     val success = new java.util.concurrent.atomic.AtomicInteger
     val latch  = new java.util.concurrent.CountDownLatch(cnt)
     for(i<-0 until cnt)
       spawn{ 
           val result= template.requestBody("seda:request",Num("124-555-1234"));
           if(Some(Bal(124.5F)) == result)success.incrementAndGet
           latch.countDown 
       }
     latch.await
     cnt must beEqualTo(success.get) 
   }

    /**
     * Shutdown
     */ 
    doAfterSpec {
      stopCamelService
      akka.actor.Actor.registry.shutdownAll
    }
}
