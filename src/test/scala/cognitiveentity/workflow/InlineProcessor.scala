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


private class InlineProcessor extends FlowActor   {
  
   var finalResult:Any = _

  /**
   * Flow is complete.
   * Return value to initiator and stop
   */
  def complete(r:Result[_]){
    finalResult = r.value
  }

 /**
  * Record a reference to the actor that initiated the flow,
  * so the result can be sent back to it on completion.
  */
  def recordOriginator {
    //noop
  }

  /**
   * Scala actor does not have any coupling to the flow or its environment.
   * The most general form is thus to send a function that creates the RPF,
   * providing maximum generality to the client. 
   * Also ensures flow initialization occurs on actor thread, increasing 
   * concurrency.
   */
  def create(a:Any):RPF = {
    a match {
      case generator:(()=>RPF) =>  generator()
    }
  }
} 

/**
 * Create an Actor to execute the flow, with its initial value.
 *
 * The initial value is sent as a message so the flow initialization
 * occurs on a different thread.
 */
object InlineProcessor {

  
  
import ValueMaps._

    val toBe = scala.collection.mutable.HashMap[CI,Any]()
 

  class DirectLookup[A,R](values:Map[A,R]) extends Lookup[A,R]{
       protected def call(arg:A):CI = {
           val ci = CorrelationAllocator()
           toBe += ci->values(arg)
           ci
         }
     }




   
 object numDLookup extends DirectLookup(numMap)
 object acctDLookup extends DirectLookup(acctMap)
 object balDLookup extends DirectLookup(balMap)
 object ppDLookup extends DirectLookup(prepaidMap)


  def apply[A](flow:A=>RPF,initial:A) = {
      val a = new InlineProcessor
      a receive Trigger({() =>flow(initial)})

     while(!toBe.isEmpty){
         val (ci,value) = toBe.head
         toBe.remove(ci)
         a receive (ci,value)
      }
    a.finalResult
  }
}