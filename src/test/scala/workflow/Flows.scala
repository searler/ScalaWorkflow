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
       concat(next,result)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }

   def concat[A,B,C,D](f:C=>D, result:End[D])(fa:(C=>RPF)=>PartialFunction[A,B]*) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1; println(count);if(count==0)result(f(arg));  else {f(arg);Done}}
     val prepped:Seq[PartialFunction[A,B]] = fa.map(pf=>pf(counter _))
     new PartialFunctionCollection(prepped)
   }


} 

object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP], result:End[List[BalanceLike]]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      concat(next,result)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})
    }

  def concat[A,B,C,D](f:C=>D, result:End[D])(fa:(C=>RPF)=>PartialFunction[A,B]*) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1; println(count);if(count==0)result(f(arg));  else {f(arg);Done}}
     val prepped:Seq[PartialFunction[A,B]] = fa.map(pf=>pf(counter _))
     new PartialFunctionCollection(prepped)
   }
   

} 