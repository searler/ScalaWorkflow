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



object NonThreadedTest  extends FlowsTest()(InlineProcessor.numDLookup,InlineProcessor.acctDLookup,InlineProcessor.balDLookup,InlineProcessor.ppDLookup)  {


  /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
protected def chMatch[A,R](flow:A=>RPF,n:A,m:matcher.Matcher[R]) {
   
    
      InlineProcessor(flow,n) match {
        case r:R => r must m
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