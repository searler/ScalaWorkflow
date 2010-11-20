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
       acctLook(pn){a:Acct => balLook(a){b:Bal => total = total + b;total}} orElse acctLook(pn){a:Acct => balLook(a){b:Bal => total = total + b;total}}
    }

   


}