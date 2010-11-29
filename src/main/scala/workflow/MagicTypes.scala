package workflow



trait RPF extends PartialFunction[Int, Any=>RPF]

case class CI(ci:Int){
   def apply[R](fn:R =>RPF):RPF = new Wrapper(ci,fn)
 }


object Done extends RPF{
   def isDefinedAt(i:Int) = false
   def apply(i:Int)= {case _ => Done}
}

class Wrapper[T](correlated:Int,fn:T=>RPF) extends RPF{
    def isDefinedAt(i:Int) = i==correlated
    def apply(i:Int)=  { a:Any=>fn(a.asInstanceOf[T])}
    override def toString = "("+correlated+ "=>" + fn +")"
}


 object Term{
    def |:[A](fci:A=>CI):WRPF[A]= new WRPF({a:A => new Wrapper(fci(a).ci,EndObject.End)})
    def |:(c:CI) = new Wrapper(c.ci,EndObject.End)
    def |:[A,B](fn:A=>B) = new TF(fn)
   }

   class WFCI[A](fn:A=>CI){
      def apply(a:A) = fn(a)
   }

   class WRPF[A](fn:A=>RPF){
      def |:(ci:CI) = new Wrapper(ci.ci,fn)
      def |:[T](g:T=>A) = new WRPF({t:T => fn(g(t))})
   }

   class TF[T,R](fn:T=>R){
     def |:[A](fci:A=>CI):WRPF[A]= new WRPF({a:A => new Wrapper(fci(a).ci,{a:T => EndObject.End(fn(a))})})
     def |:(c:CI) = new Wrapper(c.ci,{a:T => EndObject.End(fn(a))})
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