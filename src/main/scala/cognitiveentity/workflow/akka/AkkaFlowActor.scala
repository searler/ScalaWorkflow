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

import _root_.cognitiveentity.workflow.{RPF,CI,CorrelationAllocator,Result,Done,RPFCollection,FlowActor,Lookup}

/**
 * The AkkaFlowActor provides a Akka actor based implementation of
 * a flow.
 *
 * An new instance is created for each flow invocation, since each
 * has unique state.
 */ 
private abstract class AkkaFlowActor extends FlowActor with akka.actor.Actor{
   self.start

  /**
   * Return a Lookup instance that commands
   * the service actor to respond to this AkKaFlowActor instance
   * with the result of the lookup
   */
  def get[A,R](service:akka.actor.ActorRef) = {
     new Lookup[A,R]{
        def call(arg:A):CI = {
        val ci = CorrelationAllocator()
        service ! (ci,arg)
        ci
       }
     }     
  }

}

