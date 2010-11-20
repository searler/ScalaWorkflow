package workflow

object Services{
  
trait Lookup[A,R] {
  def apply[F](arg:A)(fn:R=>F):PartialFunction[String,R=>F] = {case _ => fn}
}

implicit object AccountLookup extends Lookup[Num,Acct]
implicit object BalanceLookup extends Lookup[Acct,Bal]
  

}