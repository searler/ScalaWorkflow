package workflow


case class CI(s:Int){
   def apply[R](fn:R =>RPF):RPF = new Wrapper(this,fn)
}

trait RPF extends PartialFunction[CI, Any=>RPF]

case class RPFCollection(list:Traversable[RPF]) extends RPF{
  def isDefinedAt(ci: CI): Boolean = list.exists(_.isDefinedAt(ci))
  def apply(ci:CI):Any=>RPF =  (list.filter(_.isDefinedAt(ci))).head.apply(ci)
  def toList = list toList
}

trait Lookup[A,R] {
  def apply(arg:A)(fn:R =>RPF):RPF = new Wrapper(call(arg),fn)
  def call(arg:A):CI
}

trait Terminal extends RPF{
   def isDefinedAt(ci:CI) = false
   def apply(ci:CI)= {case _ => Done}
}

object Done extends Terminal

private class Wrapper[T](correlated:CI,fn:T=>RPF) extends RPF{
    def isDefinedAt(ci:CI) = ci == correlated
    def apply(ci:CI)=  { a:Any=>fn(a.asInstanceOf[T])}
}

private case class Result[A](value:A) extends Terminal



object Flow{
   def End[A](arg:A):RPF =  new Result(arg)
   def Return[A](a: => A):Unit=>RPF = {x:Unit => new Result(a)}

   def split(fa:RPF*) = {
     new RPFCollection(fa)
   }

   def join(require:Int)(result:Unit=>RPF) = {
     var count = require
     def counter:RPF = {count-=1;if(count==0)result();  else Done}
     counter _
   }

    def inject[C,D](f:C=>D)(fa:(C=>RPF)=>RPF*)(result:Function1[D,RPF]) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1;if(count==0)result(f(arg));  else {f(arg);Done}}
     new RPFCollection(fa.map(pf=>pf(counter _)))
   }

   def first[C](fa:(C=>RPF)=>RPF*)(result:C=>RPF) = { 
     var found = false
     def counter(arg:C):RPF = {if(found) {Done}; else {found = true;result(arg)}}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

   // order of arrival
    def accummulate[C](fa:(C=>RPF)=>RPF*)(result:List[C]=>RPF) = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {buffer += arg; if(buffer.size == fa.size)result(buffer toList) else Done}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

  def scatter[A,C](look:Lookup[A,C])(result:List[C]=>RPF):Traversable[A]=>RPF = {lst:Traversable[A] => {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {buffer += arg; if(buffer.size == lst.size)result(buffer toList) else Done}
     new RPFCollection( lst.map(v=>look(v)(counter _)))
     }
  }

  def ordered[C](fa:(C=>RPF)=>RPF*)(result:Function1[List[C],RPF]) = {
      import scala.collection.immutable._
     val buffer = new scala.collection.mutable.ListBuffer[(Int,C)]() 
     def counter(index:Int)(arg:C):RPF = {buffer += index->arg; if(buffer.size == fa.size)result(SortedMap(buffer:_*).values.toList) else Done}
     new RPFCollection( fa.zipWithIndex.map(p=>p._1(counter(p._2) _)))
   }
  
  def tupled2[A,B](fa:(A=>RPF)=>RPF,fb:(B=>RPF)=>RPF)(result:Function1[(A,B),RPF]) = {
     var a:Option[A] = None
     var b:Option[B] = None
     def processA(arg:A):RPF = {a= Some(arg);if(b==None)Done else result(a.get->b.get)}
     def processB(arg:B):RPF = {b= Some(arg);if(a==None)Done else result(a.get->b.get)}
     new RPFCollection(List(fa(processA),fb(processB)))
  } 

def tupled3[A,B,C](fa:(A=>RPF)=>RPF,fb:(B=>RPF)=>RPF,fc:(C=>RPF)=>RPF)(result:Function1[(A,B,C),RPF]) = {
     var a:Option[A] = None
     var b:Option[B] = None
     var c:Option[C] = None
     def processA(arg:A):RPF = {a= Some(arg);if(b==None||c==None)Done else result((a.get,b.get,c.get))}
     def processB(arg:B):RPF = {b= Some(arg);if(a==None||c==None)Done else result((a.get,b.get,c.get))}
     def processC(arg:C):RPF = {c= Some(arg);if(a==None||b==None)Done else result((a.get,b.get,c.get))}
     new RPFCollection(List(fa(processA),fb(processB),fc(processC)))
  } 
    

}
