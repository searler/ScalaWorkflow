package workflow

import Services._
import Flow._

object SingleLineBalanceBuilder 
{
   def apply(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) =
      {pn:Num =>  acctLook(pn){balLook(_)(End)}}
}



object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]):RPF = {
       acctLook(pn){balLook(_)(End)}
    }
}


object SingleLineBalanceAsTwo{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val fun = {a:Acct => a}
      val next = {a:Acct => balLook(a)(End)}
      inject(fun)(c => acctLook(pn)(c))(next)
    }
}

object SingleLineBalanceAsTwoStripped{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      inject({a:Acct => a})(c => acctLook(pn)(c)){balLook(_:Acct)(End)}
    }
}

object TwoLineBalanceSumVar{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       var total = Bal(0)
       val  sum = {b:Bal => total += b}
       inject(sum)(
       c => acctLook(pn){balLook(_){c}} ,
       c => acctLook(pn){balLook(_){c}})(Return(total))
    }
}


object TwoLineBalanceSumVarInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(sum)(
       c => acctLook(pn){balLook(_){c}} ,
       c => acctLook(pn){balLook(_){c}})(End)
    }
}


object TwoLineBalanceVarying{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(next)(
       c => acctLook(pn){balLook(_){b:Bal => c(b+b)}} ,
       c => acctLook(pn){balLook(_){c}})(End)
    }
}



object TwoLineBalanceSequential{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       
       acctLook(pn){
         balLook(_){ 
              b1:Bal => acctLook(pn){
                balLook(_){
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
       Flow.inject(next)(
       c => acctLook(pn){balLook(_){c}} ,
       c => acctLook(pn){balLook(_){c}})(End)
    }
}

object TwoLineBalanceEfficient{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
        acctLook(pn){a:Acct=>Flow.inject(next)(
          c => balLook(a)(c) ,
          c => balLook(a)(c) )(End)
       }
      
    }
}

object TwoLineBalanceDoubled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b+b)}
       Flow.inject(sum)(
       c => acctLook(pn){balLook(_){c}} ,
       c => acctLook(pn){balLook(_){c}})(next)
    }

} 


object TwoLineBalanceDoubledInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b)}
       Flow.inject(sum)(
       c => acctLook(pn){balLook(_){b:Bal =>c(b+b)}} ,
       c => acctLook(pn){balLook(_){b:Bal =>c(b+b)}})(next)
    }

} 

object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      Flow.inject(next)(
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})(End)
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
          case Acct("alpha")  => balLook(Acct("gamma"))(next)
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
      Flow.inject(fun)( acctLook(pn))(next)
    }

}


object SingleLineBalanceOr{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val code = 12
      acctLook(pn){code match {
          case 12  => {balLook(_)(End)}
          case _ => {balLook(_) (End)}
       } 
    }
  }
}

object SingleLineBalanceAccummulate{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.accummulate[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}


object SingleLineBalanceOrdered{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.ordered[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}



object SingleLineBalanceTupled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2[Acct,Bal](
        c => acctLook(pn){c} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}

object SingleLineBalanceTupledString{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2[Acct,Bal](
        c => acctLook(pn){c} ,
        c => acctLook(pn){balLook(_){c}}) ({t:Tuple2[Acct,Bal] => End(t.toString)})
  }
}


object SingleLineBalanceFirst{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.first[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}})(End) 
  }
}

object SingleLineBalanceFirstChained{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      first[Acct]( c => acctLook(pn){c}){a:Acct => first[Bal]{d=>balLook(a){d}} (End)}
               
  }
}



object ListBalance{
   def apply(i:Int)(implicit numLook:Lookup[Int,List[Num]], acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       numLook(i)(scatter(acctLook){scatter(balLook){b:List[Bal] => End(b.foldRight(Bal(0F))(_ + _) )}})
   }
} 

object SplitJoin{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      var a1:Acct = null
      var a2:Acct = null
      val term = join(2)(Return((a1,a2)))
      split (
         acctLook(Num("124-555-1234")){a:Acct => a1=a;term()},
         acctLook(Num("333-555-1234")){a:Acct => a2=a;term()}
      )
    }
}

object SplitGather{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
    
      val term = gather[Acct](2)(End)
      split (
         acctLook(Num("124-555-1234")){term},
         acctLook(Num("333-555-1234")){term}
      )
    }
}

object SplitAny{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
    
      val term = any[Acct](End)
      split (
         acctLook(Num("124-555-1234")){term},
         acctLook(Num("333-555-1234")){term}
      )
    }
}

object Conditional{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      s match {
         case "one" => acctLook(Num("124-555-1234")){End}
         case "two" => acctLook(Num("333-555-1234")){balLook(_)(End)}
         case _ => End("unmatched")
      }
    }
}
