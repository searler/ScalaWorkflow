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

 import cognitiveentity.workflow.{CI,Lookup,Bal,Acct,Num,RPF,CorrelationAllocator,FlowsTest,Trigger}

 import cognitiveentity.workflow.ValueMaps._

/**
 * Unlike Scala, akka actors do not attach their identity to the current thread.
 * It is thus necessary to explicitly wire together the components.
 * This approach is only sensible for a serialized test environment.
 * It does provide a simple and minimalist implementation that 
 * exercises akka actors
 */
private  object current extends java.util.concurrent.atomic.AtomicReference[akka.actor.ActorRef]

private class SelfAkkaFlowActor extends RequestResponseAkkaFlowActor {
   /**
    * The wiring of the lookup to this actor instance occurs externally
    * and this implementation can thus afford to provide a minimally 
    * coupled implementation, where client send a function that
    * creates the RPF.
    */
   def create(a:Any):RPF = {
    a match {
      case generator:(()=>RPF) =>  generator()
    }
  }
}

 /**
 * Delegate the lookup to the specified Actor.
 * Represents a more realistic scenario
 */ 
private class LookupActor[A,R](values:Map[A,R]) extends Lookup[A,R]{
    //actor that performs actual lookup
    class ServiceActor extends  akka.actor.Actor{
      def receive = {
        case (id:CI,a:A) => current.get ! (id->values(a))
      }
    }
    val service = akka.actor.Actor.actorOf(new ServiceActor).start

    //request value from service actor
    def call(arg:A):CI = {
        val ci = CorrelationAllocator()
        service ! (ci,arg)
        ci
    }
}

private object numLookak extends LookupActor(numMap)
private object acctLookak extends LookupActor(acctMap)
private object balLookak extends LookupActor(balMap)
private object ppLookak extends LookupActor(prepaidMap)

/**
 * Perform FlowsTest, using akka actors and a minimalist service
 * actor implementation.
 */
object AkkaFlowsTest extends FlowsTest()(numLookak,acctLookak,balLookak,ppLookak) {

 /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
  protected def ch[A,R](flow:A=>RPF,initial:A,expected:R) {
     val a = akka.actor.Actor.actorOf[SelfAkkaFlowActor]
    
     current set a //wires services actors to this instance
             
     val response = a !!Trigger( {() => flow(initial)})
     response match {
        case Some(b:R) => b  must beEqualTo(expected)
        case _ @ x=> fail(x toString)
     }

   }
}