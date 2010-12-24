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

class Server[K,V](values:Map[K,V]) extends scala.actors.Actor {
   start
   def act(){
      loop {
        react {
           case (ci:CI,k:K) => sender ! (ci,values(k))
           case "exit" => exit
           case _ @ x => println("unexpected",x)
       }
    }
  }
}

object numServer extends Server(numMap)
object accountServer extends Server(acctMap)
object balanceServer extends Server(balMap)
object prepaidServer extends Server(prepaidMap)

implicit object numLook extends LookupActor[Int,Num](numServer)
implicit object acctLook extends LookupActor[Num,Acct](accountServer)
implicit object balLook extends LookupActor[Acct,Bal](balanceServer)
implicit object ppLook extends LookupActor[Acct,PP](prepaidServer)


  
}