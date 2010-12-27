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
 * The CI is the "correlation id" that links the response returned from
 * a service with the PartialFunction that will process it.
 *
 * apply() takes an RPF representing the portion of the flow
 * that will handle the response that matches the id and
 * wraps both the id and RPF into a Pending.
 */
case class CI(id:Int){
   def apply[R](fn:R =>RPF):RPF = new Pending(this,fn)
}

/**
 * An RPF that represents a portion of a flow that will execute when
 * the response matching the correlationId is received. 
 *
 * The RPF is only defined for specified correlationId
 *
 * apply() returns a function that casts the response to
 * the expected type and evaluates the flow portion to
 * compute the next RPF
 */
private class Pending[T](correlationId:CI,fn:T=>RPF) extends RPF{
    def isDefinedAt(ci:CI) = ci == correlationId
    def apply(ci:CI) =  {a:Any=>fn(a.asInstanceOf[T])}
}

/**
 * An RPF is a Recursive Partial Function
 * that is defined for a single CI instance
 * and provides function that computes the next
 * RPF in the flow given the response associated
 * with the CI.
 */
trait RPF extends PartialFunction[CI, Any=>RPF]

/**
 * RPFCollection is both an RPF and a simple collection of RPFs.
 *
 * It acts as an RPF when referenced from within the flow
 * implementation. In that case, it is defined if any
 * of the contained RPFs is defined for that argument.
 * Its apply is delegated to that RPF that is defined for the argument.
 * This logic is not expected to actually be implemented but
 * RPFCollection must implement RPF to satisfy the type signatures.
 * A valid implementation avoids unpleasant surprises and provides
 * generality.
 *
 * It acts as simple collection when transporting multiple RPFs
 * back to the FlowActor. 
 */
private case class RPFCollection(list:Traversable[RPF]) extends RPF{
  def isDefinedAt(ci: CI): Boolean = list.exists(_.isDefinedAt(ci))
  def apply(ci:CI):Any=>RPF =  (list.filter(_.isDefinedAt(ci))).head.apply(ci)
  def toList = list toList
}

/**
 * The Lookup trait represents the generic form of an async call to
 * an external service.
 */
trait Lookup[A,R] {
  def apply(arg:A)(fn:R =>RPF):RPF = new Pending(call(arg),fn)
  def call(arg:A):CI
}

/**
 * The endpoint of a (sub)flow, which has no further processing.
 * It does not match any CI.
 * apply should thus never be called (but a sensible fallback
 * value is returned)
 */
private trait Terminal extends RPF{
   def isDefinedAt(ci:CI) = false
   def apply(ci:CI)= {case _ => Done}
}

/**
 * Represents the completion of a subflow (generally an intermediate instance of
 * parallel subflows)
 * It only exists because all RPFs have to return another RPF. 
 */
private object Done extends Terminal

/**
 * The final result of the entire flow
 */
private case class Result[A](value:A) extends Terminal

/**
 * Defines the functions that are used to assemble a flow
 */
object Flow{

   def End[A](arg:A):RPF =  new Result(arg)
   def Return[A](a: => A):Unit=>RPF = {x:Unit => new Result(a)}

   /**
    * Evaluate flows in parallel.
    *
    * Note that no next flow is specified, each
    * sub-flow must explicitly specify the appropriate
    * function instance to rejoin the complete flow.
    */
   def split(flows:RPF*):RPF = {
     new RPFCollection(flows)
   }

   /**
    * Execute the next flow once the required
    * number of responses have been processed.
    * The values (if any) of the flows triggered by
    * responses is ignored
    *
    * Can be Used to terminate a split.
    * only useful where the subflows have side effects (e.g. recording their results
    * in variables), which is not recommended.
    * function is provided to match standard XPDL semantics
    */
   def join(required:Int)(next:Unit=>RPF) = {
     var count = required
     def counter:RPF = {
         count -= 1
         if(count == 0)
            next()
         else 
            Done
     }
     counter _
   }

   /**
    * Collect the required number of values from subflows and 
    * evaluate the next flow with a list of those values (in order
    * of receipt).
    *
    * Intended for usage with split
    * The equivalent of join, in a functional form
    */    
   def gather[C](required:Int)(next:Traversable[C]=>RPF) = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
        buffer += arg
        if(buffer.size == required)
           next(buffer toList)
        else 
           Done
     }
     counter _
   }
 
   /**
    * Evaluates the next flow with the first value resulting from
    * a split subflow.
    */
   def any[R](next:R=>RPF) = {
     var first = true
     def counter(arg:R):RPF = {
         if(first){
            first = false
            next(arg)
         }
         else 
            Done
     }
     counter _
   }

   /**
    * Evaluate the flows, applying process each result.
    * The value of the last call to process is evaluated by the next flow.
    *
    * It is expected that process assembles some state from each result
    */
    def inject[C,D](process:C=>D)(flows:(C=>RPF)=>RPF*)(next:D=>RPF):RPF = {
     var count = flows size
     def counter(arg:C):RPF = {
        count -= 1
        if(count == 0)
           next(process(arg))
        else {
           process(arg)
           Done
         }
     }
     new RPFCollection(flows.map(pf=>pf(counter _)))
   }
   
   /**
    * Evaluate n flows in parallel and feed 
    * first result to be received to the next flow
    */
   def first[C](flows:(C=>RPF)=>RPF*)(next:C=>RPF):RPF = { 
     var found = false
     def counter(arg:C):RPF = {
        if(found) 
          Done
        else {
           found = true
           next(arg)
         }
     }
     new RPFCollection(flows.map(pf=>pf(counter _)))
   }

   /**
    * Evaluate the flows, gathering their results into a list in order of receipt.
    * The next flow evaluates the complete list.
    */
    def collect[C](flows:(C=>RPF)=>RPF*)(next:Traversable[C]=>RPF):RPF = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
         buffer += arg
         if(buffer.size == flows.size)
            next(buffer toList) 
         else 
            Done
     }
     new RPFCollection( flows.map(pf=>pf(counter _)))
   }

   /**
    * Evaluate the flows, gathering their results into a list retaining order.
    * The next flow evaluates the complete list.
    */
   def ordered[C](flows:(C=>RPF)=>RPF*)(next:Traversable[C]=>RPF):RPF = {
      import scala.collection.immutable._
     val buffer = new scala.collection.mutable.ListBuffer[(Int,C)]() 
     def counter(index:Int)(arg:C):RPF = {
         buffer += index->arg
         if(buffer.size == flows.size)
            next(SortedMap(buffer:_*).values.toList)
         else 
             Done
     }
     new RPFCollection( flows.zipWithIndex.map(p=>p._1(counter(p._2) _)))
   }
  

  /**
   * Parallel calls to the service with each value of a list, gathering the responses into a list
   * for evaluation by the next flow.
   *
   * scatter returns a function that takes a list as an argument (rather than
   * simply defining the list as the third argument) to provide a cleaner
   * client API
   */
  def scatter[A,C](service:Lookup[A,C])(next:Traversable[C]=>RPF):Traversable[A]=>RPF = {lst:Traversable[A] => {
        val buffer = new scala.collection.mutable.ListBuffer[C]() 
        def counter(arg:C):RPF = {
           buffer += arg 
           if(buffer.size == lst.size)
              next(buffer toList) 
           else 
              Done
        }
        new RPFCollection( lst.map(v=>service(v)(counter _)))
     }
  }

 /**
   * Parallel evaluations of the flow with each value of a list, gathering the results into a list
   * for evaluation by the next flow.
   *
   * Generalized form of scatter
   */
  def parallel[A,C](flow:(C=>RPF)=>(A=>RPF))(next:Traversable[C]=>RPF):Traversable[A]=>RPF = {lst:Traversable[A] => {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
        buffer += arg 
        if(buffer.size == lst.size)
           next(buffer toList) 
        else 
           Done
     }
     new RPFCollection( lst.map(v=>flow(counter _)(v)))
     }
  }

  /**
   * Evaluate two flows in parallel and record their responses in a tuple
   */
  def tupled2[A,B](fa:(A=>RPF)=>RPF,fb:(B=>RPF)=>RPF)(result:((A,B))=>RPF):RPF = {
     var a:Option[A] = None
     var b:Option[B] = None
     def processA(arg:A):RPF = {
         a = Some(arg)
         if(b==None)
            Done 
         else 
            result(a.get->b.get)
     }
     def processB(arg:B):RPF = {
         b = Some(arg)
         if(a==None)
            Done
         else 
            result(a.get->b.get)
     }
     new RPFCollection(List(fa(processA),fb(processB)))
  } 

  /**
   * Evaluate three flows in parallel and record their responses in a tuple
   */
def tupled3[A,B,C](fa:(A=>RPF)=>RPF,fb:(B=>RPF)=>RPF,fc:(C=>RPF)=>RPF)(result:((A,B,C))=>RPF):RPF = {
     var a:Option[A] = None
     var b:Option[B] = None
     var c:Option[C] = None
     def processA(arg:A):RPF = {
         a= Some(arg)
         if(b==None||c==None)
            Done 
         else 
            result((a.get,b.get,c.get))
     }
     def processB(arg:B):RPF = {
         b= Some(arg)
         if(a==None||c==None)
           Done
         else 
           result((a.get,b.get,c.get))
     }
     def processC(arg:C):RPF = {
         c= Some(arg)
         if(a==None||b==None)
            Done 
         else 
            result((a.get,b.get,c.get))
     }
     new RPFCollection(List(fa(processA),fb(processB),fc(processC)))
  } 
    
}
