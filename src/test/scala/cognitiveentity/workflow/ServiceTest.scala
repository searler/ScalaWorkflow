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

object ServicesTest extends Specification{


 import Services._
import Flow._


object Extract{
   def apply[A](r:RPF):A = (r.asInstanceOf[Result[A]]).value
}


 "account lookup" in {
    val cb = acctLook(Num("124-555-1234"))(End)
    
    val a = Acct("alpha")
    val res = cb(CI(1))(a)
    val r:Acct = Extract(res)
    a must beEqualTo(r)
 } 

}