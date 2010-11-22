package workflow

import Services._

object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a){b:Bal=>b}}
    }

}

object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       concat(next)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }

   def concat[A,B,C,D](f:C=>D)(fa:(C=>Option[D])=>PartialFunction[A,B]*) = {
     var count = fa size
     def counter(arg:C):Option[D] = {count-=1;if(count==0)Some(f(arg));  else {f(arg);None}}
     val prepped:Seq[PartialFunction[A,B]] = fa.map(pf=>pf(counter))
     new PartialFunctionCollection(prepped)

    
}
   
   

}