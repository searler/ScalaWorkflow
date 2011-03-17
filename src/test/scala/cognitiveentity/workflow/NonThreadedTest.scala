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

/**
 * Execute tests defined in FlowsTest a single threaded manner, w/o any actors 
 *
 * Wires the lookups into FlowsTest 
 */
object NonThreadedTest extends FlowsTest()(InlineProcessor.numDLookup,InlineProcessor.acctDLookup,InlineProcessor.balDLookup,InlineProcessor.ppDLookup)  {
  /**
  * Execute flow and check result against matcher
  */ 
   protected def chMatch[A,R](flow:A=>RPF,initial:A,expected:matcher.Matcher[R]) {
      InlineProcessor(flow,initial) match {
         case r:R => r must expected
         case _ @ x=> fail(x toString)
      } 
   }
}