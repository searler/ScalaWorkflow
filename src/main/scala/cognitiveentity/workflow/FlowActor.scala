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
 * The FlowActor provides common  actor based implementation of
 * a flow.
 *
 * An new instance is created for each flow invocation, since each
 * has unique state.
 */ 
protected abstract class FlowActor{
  /**
   * Represents service calls for which a response is still required
   */
  var pfs:List[RPF] = Nil

  /**
   * Result applies when flow is already complete on initialization (see Conditional other)
   */
  private def processInitial(rpf:RPF) = {
       recordOriginator
       rpf match {
                   case r:Result[_] => complete(r)
                   case rc:RPFCollection => pfs = rc toList
                   case _ @ x => pfs = x :: Nil 
            }
  }

  /**
   * Event driven processing of incoming messages.
   */
   def receive:PartialFunction[Any,Unit] = {
         case (ci:CI,r:Any) => process(ci,r)
         case Trigger(a) => processInitial(create(a))
         case _ @ x=>throw new IllegalArgumentException(x toString)
   }

   /**
    * Create the starting RPF.
    * The interpretation of a is defined between the client code and
    * implementation.
    * Creation is delegated to the implementation since akka actors require
    * the flow to be wired into lookups that have references to the
    * actor instance that is hosting the flow
    */
   def create(a:Any):RPF 

   /**
    * Record the identity of the actor that initiated the flow,
    * so the final result of the flow can be sent to it.
    */
   def recordOriginator

  /**
   * Process response from service "call".
   * One and only one match is expected against the incomplete RPFs.
   * That RPF will return a value that indicates:
   * -  completion of the entire flow (with the final value)
   * - completion of some intermediate subflow
   * - RPF(s) that result from additional service call(s) 
   *
   */
  private def process(ci:CI,in:Any){
     val (matched,remaining)  = pfs.partition(pf=>pf.isDefinedAt(ci))
     pfs  = remaining
     matched.foreach{_.apply(ci)(in) match {
        case r:Result[_] => complete(r)
        case Done => //completed intermediate process (ignored)
        case rc:RPFCollection => pfs = rc.toList ::: pfs //service calls
        case r:RPF => pfs = r::pfs //another service call
        case _ @ x =>  throw new IllegalArgumentException(x toString)
       }
     }
  }

  /**
   * Flow is complete.
   * Return value to initiator and stop
   */
  def complete(r:Result[_])
}

/**
 * Causes flow to start, with the specified initial value
 */
case class Trigger[A](initial:A)

/**
 * Provide JVM unique correlation id
 */
object CorrelationAllocator{
   val id = new java.util.concurrent.atomic.AtomicInteger
   def apply() = CI(id.incrementAndGet)
}

