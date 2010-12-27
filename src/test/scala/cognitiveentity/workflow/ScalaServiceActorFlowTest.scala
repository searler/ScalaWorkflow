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
package cognitiveentity.workflow

import org.specs._

 import Services._
 import scala.actors.Actor._

/**
 * Delegate the lookup to the specified Actor.
 * Represents a more realistic scenario
 */ 
private class LookupActor[A,R](values:Map[A,R]) extends Lookup[A,R]{
    val service = new scala.actors.Actor{
     def act = {
     loop {
       react {
         case(id:CI,a:A) => sender ! (id,values(a))
       }
     }
     }
    }.start
    def call(arg:A):CI = {
        val ci = CorrelationAllocator()
        service ! (ci,arg)
        ci
    }
}
 
 private object numLookat extends LookupActor(numMap)
 private object acctLookat extends LookupActor(acctMap)
 private object balLookat extends LookupActor(balMap)
 private object ppLookat extends LookupActor(prepaidMap)

object ScalaServiceActorFlowsTest extends FlowsTest()(numLookat,acctLookat,balLookat,ppLookat)  with ScalaFlowActorImplementation {

 
}