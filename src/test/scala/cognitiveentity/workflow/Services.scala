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

object Services{
 
object CorrelationAllocator{
   val id = new java.util.concurrent.atomic.AtomicInteger
   def apply() = CI(id.incrementAndGet)
}

class LookupActor[A,R](service: scala.actors.Actor) extends Lookup[A,R]{
    def call(arg:A):CI = {val ci = CorrelationAllocator(); service ! (ci,arg); ci}
}

class LookupSelf[A,R](values:Map[A,R]) extends Lookup[A,R]{
   def call(arg:A):CI = {val ci = CorrelationAllocator(); scala.actors.Actor.self ! (ci,values(arg)); ci}
}

val numMap = Map(123->List(Num("124-555-1234"),Num("333-555-1234")))
val acctMap = Map(Num("124-555-1234") -> Acct("alpha"),Num("333-555-1234") -> Acct("beta"))
val balMap = Map(Acct("alpha") -> Bal(124.5F),Acct("beta") -> Bal(1.0F),Acct("gamma") -> Bal(11.0F))
val prepaidMap = Map(Acct("alpha") -> PP(124.5F))



implicit object numLook extends LookupSelf(numMap)
implicit object acctLook extends LookupSelf(acctMap)
implicit object balLook extends LookupSelf(balMap)
implicit object ppLook extends LookupSelf(prepaidMap)


  
}