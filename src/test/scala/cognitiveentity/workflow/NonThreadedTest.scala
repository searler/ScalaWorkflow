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


object NonThreadedTest extends Specification{

 import ValueMaps._

 val toBe = scala.collection.mutable.HashMap[CI,Any]()

 class LookupSelf[A,R](values:Map[A,R]) extends Lookup[A,R]{
       protected def call(arg:A):CI = {
           val ci = CorrelationAllocator()
           toBe += ci->values(arg)
           ci
         }
     }




   
implicit object numLookup extends LookupSelf(numMap)
implicit object acctLookup extends LookupSelf(acctMap)
implicit object balLookup extends LookupSelf(balMap)

/*
   "simple non threaded" in {
      val f1:RPF = SingleLineBalanceAsTwo(Num("124-555-1234"))
      val f2 = f1(CI(1))(Acct("alpha"))
      val f3  = f2(CI(2))(Bal(124.5F))
      f3 match {
        case r:Result[_] =>r.value must beEqualTo(Bal(124.5F))
        case _ => fail("unexpected")
      }
   }
*/
   
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

   }
}