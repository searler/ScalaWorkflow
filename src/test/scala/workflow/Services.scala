package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}
  
trait Lookup[A,R] {
  def apply[F](arg:A)(fn:R=>F):PartialFunction[Any,R=>F] 
}

trait RecordingLookup[A,R] extends Lookup[A,R]{
    def apply[F](arg:A)(fn:R=>F):PartialFunction[Any,R=>F] = {requestBuffer += arg;val CI = requestBuffer size ;{case CI => fn}}
}

implicit object AccountLookup extends RecordingLookup[Num,Acct]
implicit object BalanceLookup extends RecordingLookup[Acct,Bal]
  

}