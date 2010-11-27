package workflow



trait RPF extends PartialFunction[Int, Any=>RPF]



object Done extends RPF{
   def isDefinedAt(i:Int) = false
   def apply(i:Int)= {case _ => Done}
}

class Wrapper[T](correlated:Int,fn:T=>RPF) extends RPF{
    def isDefinedAt(i:Int) = i==correlated
    def apply(i:Int)=  { a:Any=>fn(a.asInstanceOf[T])}
    override def toString = "("+correlated+ "=>" + fn +")"
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
   def isDefinedAt(i:Int) = false
   def apply(i:Int)= {case _ => Done}
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

/*class Stop[A] extends FRPF[A]{
   def apply(a:A):RPF= Done
}
*/

/*
def cast[T](a:Any):T = a.asInstanceOf[T]
def hider[T](f:T=>RPF):Any=>RPF = { a:Any=>f(cast(a))}

def look[A,R](a:A)(f:R=>RPF):PartialFunction[String,Any=>RPF] = {case "xx" => hider(f) }


//def look[A,R](a:A)(f:R=>RPF):PartialFunction[String,R=>RPF] = {case "xx" => f }

*/