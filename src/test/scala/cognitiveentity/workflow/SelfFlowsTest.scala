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

import org.specs2.mutable._
import ValueMaps._
import org.junit.runner._
import org.specs2.runner.JUnitRunner

/**
 * Perform the lookup immediately and send the value to invoker.
 * Useful for unit testing, minimising the actor infrastructure.
 */
  private class LookupSelf[A,R](values:Map[A,R]) extends Lookup[A,R]{
   protected def call(arg:A):CI = {
       val ci = CorrelationAllocator()
       scala.actors.Actor.self ! (ci,values(arg))
       ci
    }
}

 

 private object numLookup extends LookupSelf(numMap)
 private object acctLookup extends LookupSelf(acctMap)
 private object balLookup extends LookupSelf(balMap)
 private object ppLookup extends LookupSelf(prepaidMap)

/**
 * Execute FlowTests, using ScalaActors and self contained Lookup implementations
 */
  
  

  @RunWith(classOf[JUnitRunner])
object SelfFlowsTest extends FlowsTest()(numLookup,acctLookup,balLookup,ppLookup) with CommonScalaFlowActorTest {

//Add two tests to the collection already specified in FlowsTest

"oneLineBalanceSelf" in {
   check(SingleLineBalanceBuilder(new LookupSelf(acctMap), new LookupSelf(balMap)))
  } 

"oneLineBalanceSelfPartial" in {
   //(_) is needed to provide the function and not its return value
   check(SingleLineBalance(_)(new LookupSelf(acctMap), new LookupSelf(balMap)))
  } 

}
