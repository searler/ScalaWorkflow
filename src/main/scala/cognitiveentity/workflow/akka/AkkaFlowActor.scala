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

import _root_.cognitiveentity.workflow.{RPF,CI,Result,Done,RPFCollection,FlowActor}

/**
 * The AkkaFlowActor provides a Akka actor based implementation of
 * a flow.
 *
 * An new instance is created for each flow invocation, since each
 * has unique state.
 */ 
private class AkkaFlowActor extends FlowActor with akka.actor.Actor{
  

  /**
   *  Channel which final result is sent
   */
  var originator:akka.actor.Channel[Any] = _

  /**
   * Flow is complete.
   * Return value to initiator and stop
   */
  def complete(r:Result[_]){
     originator ! r.value //respond to creator
     self.stop  //stop actor
  }

  def recordOriginator {
     originator = self.channel
  }
}

