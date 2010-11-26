package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}


 
  
trait Lookup[A,R] {
  def apply[F](arg:A)(fn:R =>RPF):RPF
  def call[F](arg:A):Int 
}

trait RecordingLookup[A,R] extends Lookup[A,R]{
    def apply[F](arg:A)(fn:R =>RPF):RPF = {requestBuffer += arg;new Wrapper(requestBuffer size,fn)}
    def call[F](arg:A):Int = {requestBuffer += arg;requestBuffer size}
}

implicit object AccountLookup extends RecordingLookup[Num,Acct]
implicit object BalanceLookup extends RecordingLookup[Acct,Bal]
implicit object PrepaidLookup extends RecordingLookup[Acct,PP]




  

}