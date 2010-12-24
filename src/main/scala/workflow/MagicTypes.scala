package workflow


case class CI(s:Int){
   def apply[R](fn:R =>RPF):RPF = new Wrapper(this,fn)
}

trait RPF extends PartialFunction[CI, Any=>RPF]

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

object Return{
   def apply[A](a: => A):Unit=>RPF = {x:Unit => new Result(a)}
}

object EndObject{
   def End[A](arg:A):RPF =  new Result(arg)
}

