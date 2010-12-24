package workflow

import Services._
import Flow._

object SingleLineBalanceBuilder 
{
   def apply(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) =
      {pn:Num =>  acctLook(pn){a:Acct => balLook(a)(End)}}
}



object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]):RPF = {
       acctLook(pn){a:Acct => balLook(a)(End)}
    }
}


object SingleLineBalanceAsTwo{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val fun = {a:Acct => a}
      val next = {a:Acct => balLook(a)(End)}
      inject(fun,next)(c => acctLook(pn)(c))
    }

}

object TwoLineBalanceSumVar{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       var total = Bal(0)
       val  sum = {b:Bal => total += b}
       Flow.inject(sum,Return(total))(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }
}


object TwoLineBalanceSumVarInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(sum,End)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }
}


object TwoLineBalanceVarying{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(next,End)(
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
       Flow.inject(next,End)(
       c => acctLook(pn){a:Acct =>  balLook(a){c}} ,
       c => acctLook(pn){a:Acct =>  balLook(a){c}})
    }
}

object TwoLineBalanceEfficient{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
        acctLook(pn){a:Acct=>Flow.inject(next,End)(
          c => balLook(a)(c) ,
          c => balLook(a)(c) )
       }
      
    }
}

object TwoLineBalanceDoubled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b+b)}
       Flow.inject(sum,next)(
       c => acctLook(pn){a:Acct => balLook(a){c}} ,
       c => acctLook(pn){a:Acct => balLook(a){c}})
    }

} 


object TwoLineBalanceDoubledInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b)}
       Flow.inject(sum,next)(
       c => acctLook(pn){a:Acct => balLook(a){b:Bal =>c(b+b)}} ,
       c => acctLook(pn){a:Acct => balLook(a){b:Bal =>c(b+b)}})
    }

} 

object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      Flow.inject(next,End)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})
    }
}

object SingleLineBalanceOrEnd{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
  
      acctLook(pn){_ match {
          case Acct("alpha")  => balLook(Acct("alpha"))(End)
          case _ @ a => balLook(a)(End)
       }
    }
  }
}



object exclusiveSplitJoinVar{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {

     var result:Bal = Bal(0F)

     def end = {End(result)}
     def next = {b:Bal => result = b;end}
    
  
      acctLook(pn){_ match {
          case Acct("alpha")  => balLook(Acct("alpha"))(next)
          case _ @ a => balLook(a)(next)
       }
    }
  }
}


object exclusiveSplitJoin{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {

      val end = {b:Bal => End(b+b)}
     val next = {b:Bal => end(b)}
    
  
      acctLook(pn){_ match {
          case Acct("alpha")  => balLook(Acct("alpha"))(next)
          case _ @ a => balLook(a)(next)
       }
    }
  }
}


object SingleLineBalanceAsPartial{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val fun = {a:Acct => a}
      val next = {a:Acct => balLook(a)(End)}
      Flow.inject(fun,next)( acctLook(pn))
    }

}


object SingleLineBalanceOr{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val code =12
      acctLook(pn){code match {
          case 12  => {balLook(_)(End)}
          case _ => {balLook(_) (End)}
       } 
    }
  }
}

object SingleLineBalanceAccummulate{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.accummulate(End)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => balLook(a){c}}) 
  }
}


object SingleLineBalanceOrdered{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.ordered(End)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => balLook(a){c}}) 
  }
}



object SingleLineBalanceTupled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2(End)(
        c => acctLook(pn){c} ,
        c => acctLook(pn){a:Acct => balLook(a){c}}) 
  }
}

object SingleLineBalanceTupledString{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2({t:Tuple2[Acct,Bal] => End(t.toString)})(
        c => acctLook(pn){c} ,
        c => acctLook(pn){a:Acct => balLook(a){c}}) 
  }
}


object SingleLineBalanceFirst{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.first(End)(
        c => acctLook(pn){a:Acct => balLook(a){c}} ,
        c => acctLook(pn){a:Acct => balLook(a){c}}) 
  }
}


 

