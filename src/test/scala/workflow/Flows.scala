package workflow

import Services._


object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a)(BalanceReturn)}
    }

}


object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], result:End[Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       PartialFunctionCollection.concat(next,result)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }



} 

object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP], result:End[List[BalanceLike]]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      PartialFunctionCollection.concat(next,result)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})
    }

 

} 