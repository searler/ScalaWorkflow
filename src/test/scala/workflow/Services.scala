package workflow

object Services{
 import scala.collection.mutable.ListBuffer


 val requestBuffer = new ListBuffer[Any]
 

 def requests = { val l = requestBuffer toList;requestBuffer clear;l}


 
  
trait Lookup[A,R] {
  def apply[F](arg:A)(fn:R =>RPF):RPF
 // def call[F](arg:A):Int 
}

trait RecordingLookup[A,R] extends Lookup[A,R]{
    def apply[F](arg:A)(fn:R =>RPF):RPF = {requestBuffer += arg;new Wrapper(requestBuffer size,fn)}
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
    def apply[F](arg:A)(fn:R =>RPF):RPF = {val ci = CorrelationAllocator(); service ! (ci,arg); println("request",ci,arg);  new Wrapper(ci,fn)}
}

import scala.actors.Actor._

val accountServer = actor {
   loop {
    react {
        case (ci:Int,Num("124-555-1234")) => println("act response",ci);sender ! (ci,Acct("alpha"))
        case "exit" => exit
        case _ => println("unexpected")
    }
  }
}

val balanceServer = actor {
   loop {
    react {
        case (ci:Int,Acct("alpha")) => println("bal response",ci); sender ! (ci,Bal(124.5F))
        case "exit" => exit
        case _ => println("unexpected")
    }
  }
}
  

}