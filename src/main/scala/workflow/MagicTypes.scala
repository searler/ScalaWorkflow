package workflow


trait Lookup[A,R] {
  def apply(arg:A)(fn:R =>RPF):RPF = new Wrapper(call(arg),fn)
  def call(arg:A):CI
}


trait RPF extends PartialFunction[CI, Any=>RPF]

case class CI(s:String){
   def apply[R](fn:R =>RPF):RPF = new Wrapper(this,fn)
 }


object Done extends RPF{
   def isDefinedAt(ci:CI) = false
   def apply(i:CI)= {case _ => Done}
}

class Wrapper[T](correlated:CI,fn:T=>RPF) extends RPF{
    def isDefinedAt(ci:CI) = ci == correlated
    def apply(ci:CI)=  { a:Any=>fn(a.asInstanceOf[T])}
}


/*
class Accum[A] extends Function1[A,A]{
   type summable = {def +=(a:A):A}
   var v:summable = _
   def apply(a:summable) = {
     v += a
     v
   } 
}
*/

case class Result[A](value:A) extends RPF{
   def isDefinedAt(ci:CI) = false
   def apply(ci:CI)= {case _ => Done}
}

object Extract{
   def apply[A](r:RPF):A = (r.asInstanceOf[Result[A]]).value
}

object Return{
   def apply[A](a: => A):Unit=>RPF = {x:Unit => new Result(a)}
}



object EndObject{
   def End[A](arg:A):RPF =  new Result(arg)
}

