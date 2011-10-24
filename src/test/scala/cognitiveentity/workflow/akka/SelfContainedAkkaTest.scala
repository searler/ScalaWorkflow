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
 * Simplest sensible implementation of an akka based flow.
 * All services are directly referenced.
 *
 * 
 * @author Richard Searle
 */
package cognitiveentity.workflow.akka

import cognitiveentity.workflow.{Bal,PP,Acct,Num}

/**
 * Actor that implements all the lookups, immediately
 * replying with values drawn from the corresponding maps
 */
private class SelfContainedService extends akka.actor.Actor{
   import cognitiveentity.workflow.CI
   import cognitiveentity.workflow.ValueMaps._
   def receive = {
     case (ci:CI,id:Int)    => self.reply((ci,numMap(id)))
     case (ci:CI,num:Num)   => self.reply((ci,acctMap(num)))
     case (ci:CI,acct:Acct) => self.reply((ci,balMap(acct)))
   }
}
 
/**
 * Singleton implementation representing the services required
 * by the flows
 */
private object SelfContainedService{
  val sa = akka.actor.Actor.actorOf[SelfContainedService].start
}

/**
 * FlowsSwitch creates flows instance that uses the implicitly
 * provided lookups. 
 * Those lookups are are connected to a single actor that immediately
 * replies with the appropriate value. 
 * 
 */
private class SelfContainedLauncher extends RequestResponseAkkaFlowActor{  
 
  override def create(a:Any) =  cs(a)
  
  import SelfContainedService._
  
  private implicit val callNum = get[Int,List[Num]](sa)
  private implicit val callAcct = get[Num,Acct](sa)
  private implicit val callBal = get[Acct,Bal](sa)
  private implicit val callPP = get[Acct,PP](sa)

  private val cs = new cognitiveentity.workflow.FlowsSwitch
}

/**
 * Create an actor instance and initiate 
 * a flow identified by (and started with) the 
 * initial value
 */
private object SelfContainedLauncher {
   import cognitiveentity.workflow.Trigger
   def apply[A](initial:A) = {
     val actRef = akka.actor.Actor.actorOf[SelfContainedLauncher]
     actRef !! Trigger(initial)
   }
} 

/**
 * Flows are accessed via FlowsSwitch, limiting the scope of testing.
 */
object SelfContainedAkkaTest extends org.specs2.mutable.SpecificationWithJUnit {
    
    "num" in  {
     Some(List(Num("124-555-1234"),Num("333-555-1234")))  must beEqualTo(SelfContainedLauncher(123))
    }

    "bal" in  {
     Some(Bal(124.5F))  must beEqualTo(SelfContainedLauncher(Num("124-555-1234")))
    }

   step {
      akka.actor.Actor.registry.shutdownAll
      success
    }

}


