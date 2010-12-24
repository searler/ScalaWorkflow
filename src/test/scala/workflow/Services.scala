package workflow

object Services{
 
object CorrelationAllocator{
   val id = new java.util.concurrent.atomic.AtomicInteger
   def apply() = CI(id.incrementAndGet)
}

class LookupActor[A,R](service: scala.actors.Actor) extends Lookup[A,R]{
    def call(arg:A):CI = {val ci = CorrelationAllocator(); service ! (ci,arg); ci}
}

class LookupSelf[A,R](values:Map[A,R]) extends Lookup[A,R]{
   def call(arg:A):CI = {val ci = CorrelationAllocator(); scala.actors.Actor.self ! (ci,values(arg)); ci}
}

val numMap = Map(123->Num("124-555-1234"))
val acctMap = Map(Num("124-555-1234") -> Acct("alpha"))
val balMap = Map(Acct("alpha") -> Bal(124.5F))
val prepaidMap = Map(Acct("alpha") -> PP(124.5F))



implicit object numLook extends LookupSelf(numMap)
implicit object acctLook extends LookupSelf(acctMap)
implicit object balLook extends LookupSelf(balMap)
implicit object ppLook extends LookupSelf(prepaidMap)


  
}