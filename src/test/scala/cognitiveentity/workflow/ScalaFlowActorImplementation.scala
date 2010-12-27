
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

trait ScalaFlowActorImplementation extends Specification{

import scala.actors.Actor._

/**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
protected def ch[A,R](flow:A=>RPF,n:A,expected:R) {
    ScalaFlowActor(flow,n)
    receiveWithin(1000L){
       case b:R => b  must beEqualTo(expected)
       case scala.actors.TIMEOUT => fail("timeout")
       case _ @ x=> fail(x toString)
      }
 }
}