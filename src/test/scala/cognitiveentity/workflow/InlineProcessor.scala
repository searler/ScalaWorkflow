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
  
   /**
    * Capture final flow result for checking in the test
    */
   var finalResult:Any = _

  /**
   * Flow is complete.
   * Return value to initiator and stop
   */
  def complete(r:Result[_]){
    finalResult = r.value
  }

  def recordOriginator {
    //noop
  }

  /**
   * Initial execution of flow occurred immediately
   * upon construction, so this is a no-op
   */
  def create(a:Any):RPF = {
     a.asInstanceOf[RPF]
  }
} 

/**
 * Create an Actor to execute the flow, with its initial value.
 *
 * The initial value is sent as a message so the flow initialization
 * occurs on a different thread.
 */
object InlineProcessor {
  
  

  /**
   * Unprocessed results from the external lookups
   */
  var toBe:List[(CI,Any)] = Nil
 
   /**
    * Simulate external lookup, recording the result for processing
    */
   class DirectLookup[A,R](values:Map[A,R]) extends Lookup[A,R]{
       protected def call(arg:A):CI = {
           val ci = CorrelationAllocator()
           toBe = ci->values(arg) :: toBe
           ci
       }
   }

   import ValueMaps._
   
   object numDLookup extends DirectLookup(numMap)
   object acctDLookup extends DirectLookup(acctMap)
   object balDLookup extends DirectLookup(balMap)
   object ppDLookup extends DirectLookup(prepaidMap)

   /**
    * Execute flow, processing each tuple returned from Lookup
    * until final result is populated
    * Check the final result against the expected value
    */
   def apply[A](flow:A=>RPF,initial:A) = {
      val processor = new InlineProcessor
      processor receive Trigger(flow(initial))


      while(processor.finalResult == null){
         val copy = toBe
         toBe = Nil
         copy foreach {processor receive _}
      }

    processor.finalResult
  }
}
