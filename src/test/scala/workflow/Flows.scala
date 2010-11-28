package workflow

import Services._
import EndObject._

class SingleLineBalanceBuilder(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) extends Function1[Num,RPF]{
    def apply(pn:Num)= acctLook(pn){a:Acct => balLook(a)(End)}
    

}

object SingleLineBalance{
    

    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){a:Acct => balLook(a)(End)}
    }
}
/*
object SingleLineBalancePipeline{
object Functional {
    class PipedObject[T] private[Functional] (value:T)
    {
        def |>[R] (f : T => R) = f(this.value)
    }
    implicit def toPiped[T] (value:T) = new PipedObject[T](value)
    }

     import Functional._
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn) _|>{a:Acct => balLook(a) _} |> End
    }
}*/

object SingleLineBalanceAsTwo{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val fun = {a:Acct => a}
      val next = {a:Acct => balLook(a)(End)}
      PartialFunctionCollection.concat(fun,next)(c => acctLook(pn)(c))
    }

}

object TwoLineBalanceSumVar{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       var total = Bal(0)
       val  sum = {b:Bal => total += b}
       PartialFunctionCollection.concat(sum,Return(total))(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }
}


object TwoLineBalanceVarying{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       PartialFunctionCollection.concat(next,End)(
       c => acctLook(pn){a:Acct => balLook(a){b:Bal => c(b+b)}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }
}

object TwoLineBalanceSequential{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       
       acctLook(pn){
         a:Acct => balLook(a){ 
              b1:Bal => acctLook(pn){
                a:Acct => balLook(a){
                   b2:Bal => End(b1+b2)}
              }
         }
      }
  }
}

object TwoLineBalanceSequentialOptimized{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       acctLook(pn){
         a:Acct => 
         balLook(a){ 
              b1:Bal => {
                balLook(a){
                   b2:Bal => End(b1+b2)
                }
              }
         }
      }
  }
}

object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       PartialFunctionCollection.concat(next,End)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }
}

object TwoLineBalanceEfficient{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
        acctLook(pn){a:Acct=>PartialFunctionCollection.concat(next,End)(
          c => balLook(a)(c) ,
          c => balLook(a)(c) )
       }
      
    }
}

object TwoLineBalanceDoubled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b+b)}
       PartialFunctionCollection.concat(sum,next)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }

} 

object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      PartialFunctionCollection.concat(next,End)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})
    }

 

} 