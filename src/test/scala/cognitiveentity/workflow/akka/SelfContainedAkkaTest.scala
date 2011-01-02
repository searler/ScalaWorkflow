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
package cognitiveentity.workflow.akka

import cognitiveentity.workflow.{Bal,PP,Acct,Num}

private class SelfContainedService extends akka.actor.Actor{
   import cognitiveentity.workflow.CI
   import cognitiveentity.workflow.Services._
   def receive = {
     case (ci:CI,id:Int)    => self.reply((ci,numMap(id)))
     case (ci:CI,num:Num)   => self.reply((ci,acctMap(num)))
     case (ci:CI,acct:Acct) => self.reply((ci,balMap(acct)))
   }
}
 
private object SelfContainedService{
  val sa = akka.actor.Actor.actorOf[SelfContainedService].start
}

private class SelfContainedLauncher extends RequestResponseAkkaFlowActor{  
 
  override def create(a:Any) =  cs(a)
  
  import SelfContainedService._
  
  implicit val callNum = get[Int,List[Num]](sa)
  implicit val callAcct = get[Num,Acct](sa)
  implicit val callBal = get[Acct,Bal](sa)
  implicit val callPP = get[Acct,PP](sa)

  val cs = new cognitiveentity.workflow.FlowsSwitch
}

private object SelfContainedLauncher {
   import cognitiveentity.workflow.Trigger
   def apply[A](initial:A) = {
     val actRef = akka.actor.Actor.actorOf[SelfContainedLauncher]
     actRef !! Trigger(initial)
   }
} 


object SelfContainedAkkaTest extends org.specs.Specification {
    "num" in  {
     Some(List(Num("124-555-1234"),Num("333-555-1234")))  must beEqualTo(SelfContainedLauncher(123))
    }

    "bal" in  {
     Some(Bal(124.5F))  must beEqualTo(SelfContainedLauncher(Num("124-555-1234")))
    }

   doAfterSpec {
      akka.actor.ActorRegistry.shutdownAll
    }
}


