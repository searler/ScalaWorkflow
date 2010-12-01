package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}

  
trait RecordingLookup[A,R] extends Lookup[A,R]{
    def call(arg:A):CI = {requestBuffer += arg; new CI(requestBuffer.size.toString)}
}

implicit object NumLookup extends RecordingLookup[Int,Num]
implicit object AccountLookup extends RecordingLookup[Num,Acct]
implicit object BalanceLookup extends RecordingLookup[Acct,Bal]
implicit object PrepaidLookup extends RecordingLookup[Acct,PP]

object CorrelationAllocator{
   val id = new java.util.concurrent.atomic.AtomicInteger
   def apply() = CI(id.incrementAndGet.toString)
}

class LookupActor[A,R](service: scala.actors.Actor) extends Lookup[A,R]{
    def call(arg:A):CI = {val ci = CorrelationAllocator(); service ! (ci,arg);   ci}
}

import scala.actors.Actor._

val accountServer = actor {
   loop {
    react {
        case (ci:CI,Num("124-555-1234")) => sender ! (ci,Acct("alpha"))
        case "exit" => exit
        case _ @ x => println("unexpected accountServer",x)
    }
  }
}

val balanceServer = actor {
   loop {
    react {
        case (ci:CI,Acct("alpha")) =>  sender ! (ci,Bal(124.5F))
        case "exit" => exit
        case _ @ x => println("unexpected balanceServer",x)
    }
  }
}
  

}