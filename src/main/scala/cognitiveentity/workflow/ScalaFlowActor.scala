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

/**
 * The FlowActor provides a Scala actor based implementation of
 * a flow.
 *
 * An new instance is created for each flow invocation, since each
 * has unique state.
 */ 
private class ScalaFlowActor extends FlowActor with scala.actors.Actor   {
  start

  /**
   *  Actor which created this instance and will receive the
   *  final result
   */
  var originator:Option[scala.actors.OutputChannel[Any]] = None

  /**
   * Event driven processing of incoming messages.
   */
  def act(){
     loop {
       react {
         receive
       }
     }
  }

  /**
   * Flow is complete.
   * Return value to initiator and stop
   */
  def complete(r:Result[_]){
     originator.get ! r.value //respond to creator
     exit  //stop actor
  }

 /**
  * Record a reference to the actor that initiated the flow,
  * so the result can be sent back to it on completion.
  */
  def recordOriginator {
     originator = Some(sender)
  }

  /**
   * Scala actor does not have any coupling to the flow or its environment.
   * The most general form is thus to send a function that creates the RPF,
   * providing maximum generality to the client. 
   * Also ensures flow initialization occurs on actor thread, increasing 
   * concurrency.
   */
  def create(a:Any):RPF = {
    a match {
      case generator:(()=>RPF) =>  generator()
    }
  }
}

/**
 * Create an Actor to execute the flow, with its initial value.
 *
 * The initial value is sent as a message so the flow initialization
 * occurs on a different thread.
 */
object ScalaFlowActor {
  def apply[A](flow:A=>RPF,initial:A) = {
      val a = new ScalaFlowActor
      a ! Trigger({() =>flow(initial)})
  }
}