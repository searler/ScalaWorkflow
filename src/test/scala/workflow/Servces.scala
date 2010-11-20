package workflow

object Services{
  
trait Lookup[A,R,F] {
  def apply[F](arg:A)(fn:R=>F):PartialFunction[String,R=>F] = {case _ => fn}
}

implicit object AccountLookup {
  def apply[T](pn:Num)(fn:Acct=>T):PartialFunction[String,Acct=>T] = {case _ => fn}
}
implicit object BalanceLookup {
   def apply[T](ac:Acct)(fn:Bal=>T):PartialFunction[String,Bal=>T] = { case _ => fn}
}

}