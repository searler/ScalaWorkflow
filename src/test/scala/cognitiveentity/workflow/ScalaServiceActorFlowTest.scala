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

object ScalaServiceActorFlowsTest extends Specification {

 import Services._
 import scala.actors.Actor._

/**
 * Delegate the lookup to the specified Actor.
 * Represents a more realistic scenario
 */ 
class LookupActor[A,R](values:Map[A,R]) extends Lookup[A,R]{
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
 
implicit object numLook extends LookupActor(numMap)
implicit object acctLook extends LookupActor(acctMap)
implicit object balLook extends LookupActor(balMap)
implicit object ppLook extends LookupActor(prepaidMap)

 /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
  private def ch[A,R](flow:A=>RPF,n:A,expected:R) {
    ScalaFlowActor(flow,n)
    receiveWithin(1000L){
       case b:R => b  must beEqualTo(expected)
       case scala.actors.TIMEOUT => fail("timeout")
       case _ @ x=> fail(x toString)
      }
 }

 /**
  * Test a flow that takes Num("124-555-1234") and returns an R
  */
 private def chk[R](flow:Num=>RPF,expected:R) =ch(flow,Num("124-555-1234"),expected)

 /**
  * Test a flow that takes a Num and returns a Bal, with a default
  * of Bal(124.5F)
  */
 private def check(flow:Num=>RPF,expected:Bal=Bal(124.5F)) = chk(flow,expected)

  "oneLineBalance" in {
   check(SingleLineBalance(_))
  } 


 "twoLineBalance" in {
   check( TwoLineBalance(_),Bal(249F))
  } 
}