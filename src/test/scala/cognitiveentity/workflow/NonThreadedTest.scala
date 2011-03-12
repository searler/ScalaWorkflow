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

import ValueMaps._

 object Holder{
    val toBe = scala.collection.mutable.HashMap[CI,Any]()
 }

 private class DirectLookup[A,R](values:Map[A,R]) extends Lookup[A,R]{
       protected def call(arg:A):CI = {
           val ci = CorrelationAllocator()
           Holder.toBe += ci->values(arg)
           ci
         }
     }




   
private object numDLookup extends DirectLookup(numMap)
private object acctDLookup extends DirectLookup(acctMap)
private object balDLookup extends DirectLookup(balMap)
private object ppDLookup extends DirectLookup(prepaidMap)

object NonThreadedTest  extends FlowsTest()(numDLookup,acctDLookup,balDLookup,ppDLookup) {


  /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
protected def ch[A,R](flow:A=>RPF,n:A,expected:R) {
   
      var rpf = flow(n)
       while(!Holder.toBe.isEmpty){
         val (ci,value) = Holder.toBe.head
         Holder.toBe.remove(ci)
         rpf = rpf(ci)(value) 
      }
      
      rpf match {
        case r:Result[_] =>r.value must beEqualTo(expected)
       case _ @ x=> fail(x toString)
      }
   
  
 }
 
/*

   
   "improved non threaded" in {
      var rpf = SingleLineBalanceAsTwo(Num("124-555-1234"))
      while(!toBe.isEmpty){
         val (ci,value) = toBe.head
         toBe.remove(ci)
         rpf = rpf(ci)(value) 
      }
      
      rpf match {
        case r:Result[_] =>r.value must beEqualTo(Bal(124.5F))
        case _ => fail("unexpected")
      }

   } */
}