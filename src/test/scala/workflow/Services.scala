package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}


 class CI(ci:Int){
   def apply[R](fn:R =>RPF):RPF = new Wrapper(ci,fn)
 }
 
  
trait Lookup[A,R] {
  def apply(arg:A):CI
 // def call[F](arg:A):Int 
}

trait RecordingLookup[A,R] extends Lookup[A,R]{
    def apply(arg:A):CI = {requestBuffer += arg; new CI(requestBuffer size)}
  //  def call[F](arg:A):Int = {requestBuffer += arg;requestBuffer size}
}

implicit object AccountLookup extends RecordingLookup[Num,Acct]
implicit object BalanceLookup extends RecordingLookup[Acct,Bal]
implicit object PrepaidLookup extends RecordingLookup[Acct,PP]

object CorrelationAllocator{
   val id = new java.util.concurrent.atomic.AtomicInteger
   def apply() = id incrementAndGet
}

class LookupActor[A,R](service: scala.actors.Actor) extends Lookup[A,R]{
    def apply(arg:A):CI = {val ci = CorrelationAllocator(); service ! (ci,arg);   new CI(ci)}
}

import scala.actors.Actor._

val accountServer = actor {
   loop {
    react {
        case (ci:Int,Num("124-555-1234")) => sender ! (ci,Acct("alpha"))
        case "exit" => exit
        case _ => println("unexpected")
    }
  }
}

val balanceServer = actor {
   loop {
    react {
        case (ci:Int,Acct("alpha")) =>  sender ! (ci,Bal(124.5F))
        case "exit" => exit
        case _ => println("unexpected")
    }
  }
}
  

}