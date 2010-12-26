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

object Flow{
   def End[A](arg:A):RPF =  new Result(arg)
   def Return[A](a: => A):Unit=>RPF = {x:Unit => new Result(a)}

   def split(fa:RPF*):RPF = {
     new RPFCollection(fa)
   }

   def join(require:Int)(result:Unit=>RPF) = {
     var count = require
     def counter:RPF = {
         count-=1
         if(count==0)
            result()
         else 
            Done
     }
     counter _
   }

   def any[R](result:R=>RPF) = {
     var first = true
     def counter(arg:R):RPF = {
         if(first){
            first = false
            result(arg)
         }
         else 
            Done
     }
     counter _
   }

  def gather[C](require:Int)(result:Traversable[C]=>RPF) = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
        buffer += arg
        if(buffer.size == require)
           result(buffer toList)
        else 
           Done
     }
     counter _
   }

    def inject[C,D](f:C=>D)(fa:(C=>RPF)=>RPF*)(result:D=>RPF):RPF = {
     var count = fa size
     def counter(arg:C):RPF = {
        count-=1
        if(count==0)
           result(f(arg))
        else {
           f(arg)
           Done
         }
     }
     new RPFCollection(fa.map(pf=>pf(counter _)))
   }

   def first[C](fa:(C=>RPF)=>RPF*)(result:C=>RPF):RPF = { 
     var found = false
     def counter(arg:C):RPF = {
        if(found) 
          Done
        else {
           found = true
           result(arg)
         }
     }
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

   // order of arrival
    def collect[C](fa:(C=>RPF)=>RPF*)(result:Traversable[C]=>RPF):RPF = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
         buffer += arg
         if(buffer.size == fa.size)
            result(buffer toList) 
         else 
            Done
     }
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

  def parallel[A,C](f:(C=>RPF)=>(A=>RPF))(result:Traversable[C]=>RPF):Traversable[A]=>RPF = {lst:Traversable[A] => {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
        buffer += arg 
        if(buffer.size == lst.size)
           result(buffer toList) 
        else 
           Done
     }
     new RPFCollection( lst.map(v=>f(counter _)(v)))
     }
  }


  def scatter[A,C](look:Lookup[A,C])(result:Traversable[C]=>RPF):Traversable[A]=>RPF = {lst:Traversable[A] => {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {
        buffer += arg 
        if(buffer.size == lst.size)
           result(buffer toList) 
        else 
           Done
     }
     new RPFCollection( lst.map(v=>look(v)(counter _)))
     }
  }

 

  def ordered[C](fa:(C=>RPF)=>RPF*)(result:Traversable[C]=>RPF):RPF = {
      import scala.collection.immutable._
     val buffer = new scala.collection.mutable.ListBuffer[(Int,C)]() 
     def counter(index:Int)(arg:C):RPF = {
         buffer += index->arg
         if(buffer.size == fa.size)
            result(SortedMap(buffer:_*).values.toList)
         else 
             Done
     }
     new RPFCollection( fa.zipWithIndex.map(p=>p._1(counter(p._2) _)))
   }
  
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
