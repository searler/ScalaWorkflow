package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}

type RR = PartialFunction[Int,Any=>RPF]   

 def cast[T](a:Any):T = a.asInstanceOf[T]
 def hider[T](f:T=>RPF):Any=>RPF = { a:Any=>f(cast(a))}
  
trait Lookup[A,R] {
  def apply[F](arg:A)(fn:R =>RPF):RPF
  def call[F](arg:A):Int 
}

trait RecordingLookup[A,R] extends Lookup[A,R]{
    def apply[F](arg:A)(fn:R =>RPF):RPF = {requestBuffer += arg;val CI = requestBuffer size ; new Wrapper(12,fn)}
    def call[F](arg:A):Int = {requestBuffer += arg;val CI = requestBuffer size ; CI}
}

implicit object AccountLookup extends RecordingLookup[Num,Acct]
implicit object BalanceLookup extends RecordingLookup[Acct,Bal]



class End[A] extends Function1[A,RPF]{
   var v:Option[A] = None
   def apply(arg:A):RPF = {
       v = Some(arg)
        Done
       }
   def get:A = {
     val r:A = v.get
     v = None
     r
   }
}

implicit object AccountReturn extends End[Acct]
implicit object BalanceReturn extends End[Bal]

  

}