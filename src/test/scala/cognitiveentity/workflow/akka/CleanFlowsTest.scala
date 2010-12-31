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

 import cognitiveentity.workflow.{CI,Lookup,Bal,PP,Acct,Num,RPF,CorrelationAllocator,FlowsTest,Trigger}


 import cognitiveentity.workflow.Services._

class Service extends akka.actor.Actor{
   def receive = {
     case (ci:CI,id:Int)=> self.reply((ci,List(Num("ABA"))))
     case (ci:CI,num:Num) => self.reply((ci,Acct("acct")))
   }
}
 
object Service{
  val sa = akka.actor.Actor.actorOf[Service].start
}

private class SwitchingAkkaFlowActor extends AkkaFlowActor{  
 
 override def create(a:Any):(()=>RPF) = {
     cs(a)
   }
  

import Service._
  
  implicit val callNum = get[Int,List[Num]](sa)
  implicit val callAcct = get[Num,Acct](sa)
  implicit val callBal = get[Acct,Bal](sa)
  implicit val callPP = get[Acct,PP](sa)

  val cs = new FlowsSwitch
}


 class FlowsSwitch(implicit numLook:Lookup[Int,List[Num]],acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) {
   import cognitiveentity.workflow.Flow._
   def apply(a:Any) = {
      a match {
         case id:Int => { ()=>numLook(id)(End)}
         case num:Num => {() => acctLook(num)(End)}
      }
   }
 }

 object Launcher {
   def apply[A](initial:A) = {
     val actRef = akka.actor.Actor.actorOf[SwitchingAkkaFlowActor]
     actRef !! Trigger(initial)
   }
 } 

 import org.specs._

 object LauncherTest extends Specification {
    "id" in  {
     Some(List(Num("ABA")))  must beEqualTo(Launcher(134))
    }

    "acct" in  {
     Some(Acct("acct"))  must beEqualTo(Launcher(Num("ABA")))
    }
 }


