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

 import cognitiveentity.workflow.{CI,Lookup,Bal,Acct,Num,RPF,CorrelationAllocator,FlowsTest}

 import cognitiveentity.workflow.Services._

private  object current extends java.util.concurrent.atomic.AtomicReference[akka.actor.ActorRef]

 /**
 * Delegate the lookup to the specified Actor.
 * Represents a more realistic scenario
 */ 
private class LookupActor[A,R](values:Map[A,R]) extends Lookup[A,R]{
    class ServiceActor extends  akka.actor.Actor{
      def receive = {
        case (id:CI,a:A) => current.get ! (id->values(a))
      }
    }
    val service = akka.actor.Actor.actorOf(new ServiceActor).start
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

object AkkaFlowsTest extends FlowsTest()(numLookak,acctLookak,balLookak,ppLookak) {

 /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
  protected def ch[A,R](flow:A=>RPF,initial:A,expected:R) {
     val a = akka.actor.Actor.actorOf[AkkaFlowActor]
    // a.start
     current set a
             
     val response = a !! {() => flow(initial)}
     response match {
        case Some(b:R) => b  must beEqualTo(expected)
        case _ @ x=> fail(x toString)
     }

   }
}