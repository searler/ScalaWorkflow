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

class FlowActor[A](flow:A=>RPF) extends scala.actors.Actor{
  var pfs:List[RPF] = Nil
  var originator:scala.actors.OutputChannel[Any] = _
  def act(){
     loop {
     react {
       case (ci:CI,r:Any) => process(ci,r) //process incoming service response
       case arg:A => { 
                        originator = sender
                        flow(arg) match {
                              case r:Result[_] => originator ! r.value; exit //immediate done
                              case rc:RPFCollection => pfs = rc toList
                              case _ @ x => pfs = x :: Nil 
                            }
                     }
       case _ @ x=> println("unexpected",x)
     }
   }
  }

  def process(ci:CI,in:Any){
     val (matched,remaining)  = pfs.partition(pf=>pf.isDefinedAt(ci))
     pfs  = remaining
     matched.foreach{_.apply(ci)(in) match {
        case r:Result[_] => originator ! r.value; exit //all done
        case Done => //ignored intermediate process ended
        case rc:RPFCollection => pfs = rc.toList ::: pfs
        case r:RPF => pfs = r::pfs //another service call
        case _ @ x =>  println("unexpected",x)
       }
     }
  }
}

object FlowActor {
  def apply[A](flow:A=>RPF,initial:A) = {
      val a = new FlowActor(flow)
      a.start
      a ! initial
  }
}