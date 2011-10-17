
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

/**
 * A trait to support Specs testing with instances of
 * ScalaFlowActor
 */
trait CommonScalaFlowActorTest extends Specification{

import scala.actors.Actor._

/**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
protected def chMatch[A,R](flow:A=>RPF,n:A,m:org.specs2.matcher.Matcher[R]): org.specs2.execute.Result= {
    ScalaFlowActor(flow,n)
    receiveWithin(1000L){
       case b:R => b  must m
       case scala.actors.TIMEOUT => failure("timeout")
       case _ @ x=> failure(x toString)
      }
 }
}
