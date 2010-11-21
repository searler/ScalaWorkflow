package workflow

import Services._

object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a){b:Bal=>b}}
    }

}

object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       
      // acctLook(pn){a:Acct => balLook(a){b:Bal=> total= total+ b.v; Bal(total)}}
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       concat(next)({c => acctLook(pn){a:Acct => balLook(a){c}}} , {c=>acctLook(pn){a:Acct => balLook(a){c}}})
    }

   //fa(0)(f) orElse fa(1)(f) orElse Map()
   def concat[A,B,C,D](f:C=>D)(fa:(C=>Option[D])=>PartialFunction[A,B]*) = {
     var count = fa size
     def counter(arg:C):Option[D] = {count-=1;if(count==0)Some(f(arg));  else {f(arg);None}}
     var s =  (fa.head){counter}
     for(x<-fa.tail) s = s.orElse(x(counter))
     s    

     
    
}
   
   

}