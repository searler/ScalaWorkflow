package workflow

import Services._

object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a){b:Bal=>b}}
    }

}

object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       var total = Bal(0)
      // acctLook(pn){a:Acct => balLook(a){b:Bal=> total= total+ b.v; Bal(total)}}
       val  common = {b:Bal => total = total + b;total}
       concat(common)({c => acctLook(pn){a:Acct => balLook(a)(c)}} , {c=>acctLook(pn){a:Acct => balLook(a)(c)}})
    }

   //fa(0)(f) orElse fa(1)(f) orElse Map()
   def concat[A,B,C,D](f:C=>D)(fa:(C=>D)=>PartialFunction[A,B]*) = {
     var s =  (fa.head)(f)
     for(x<-fa.tail) s = s.orElse(x(f))
     s    
    
}
   
   

}