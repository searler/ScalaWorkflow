package workflow

import Services._


object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a)(BalanceReturn)}
    }

}

object SingleLineBalanceAsTwo{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val next = {a:Acct => a}
      val result = {a:Acct => balLook(a)(BalanceReturn)}
      PartialFunctionCollection.concat(next,result)(c => acctLook(pn)(c))
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

object TwoLineBalanceEfficient{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], result:End[Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       def  balances(a:Acct) = {PartialFunctionCollection.concat(next,result)(
          c => balLook(a)(c) ,
          c => balLook(a)(c) )
       }
       acctLook(pn)(balances)
    }
}

object TwoLineBalanceDoubled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], result:End[Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       val double = {b:Bal => result(b+b)}
       PartialFunctionCollection.concat(next,double)(
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