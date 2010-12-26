/* Copyright (c) 2010 Richard Searle
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Richard Searle
 */
package cognitiveentity.workflow

import Services._
import Flow._

/**
 * Simple lookup of balance from phone number (via account)
 * Explicitly returns a function value, which simplifies
 * the calling code but is more complex at the point of definition.
 */
object SingleLineBalanceBuilder 
{
   def apply(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) =
      {pn:Num =>  acctLook(pn){balLook(_)(End)}}
}

/**
 * Simple lookup of balance from phone number (via account)
 * Complicates the calling code, but provides simpler definition.
 * Given that calling code is part of infrastructure and the flow is
  defined by the framework clients, this is a better trade-off.
 */
object SingleLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]):RPF = {
       acctLook(pn){balLook(_)(End)}
    }
}

/**
 * Lookup Balance from Phone number
 * Trivial usage of inject (serves no purpose other
 * than demostrating its usage)
 */
object SingleLineBalanceAsTwo{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      val fun = {a:Acct => a}
      val next = {a:Acct => balLook(a)(End)}
      inject(fun)(c => acctLook(pn)(c))(next)
    }
}

/**
 * SingleLineBalanceAsTwo, in a more compact form.
 */
object SingleLineBalanceAsTwoStripped{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      inject({a:Acct => a})(c => acctLook(pn)(c)){balLook(_:Acct)(End)}
    }
}

/**
 *  Perform phone number -> balance lookup twice in parallel,
 *  and the sum the results.
 *  Demostrate how per flow state can be used.
 */
object TwoLineBalanceSumVar{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       var total = Bal(0)
       val  sum = {b:Bal => total += b}
       inject(sum)(
          c => acctLook(pn){balLook(_){c}} ,
          c => acctLook(pn){balLook(_){c}}
        ) (Return(total))
    }
}

/**
 * Same as TwoLineBalanceSumVar, 
 * demonstrating alternative means to maintain state
 */
object TwoLineBalanceSumVarInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(sum)(
          c => acctLook(pn){balLook(_){c}} ,
          c => acctLook(pn){balLook(_){c}}
       ) (End)
    }
}

/**
 * Similar to TwoLineBalanceSumVar, 
 * demonstrating how per case logic could be embedded.
 * Returns the balance * 3
 */
object TwoLineBalanceVarying{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(next)(
         c => acctLook(pn){balLook(_){b:Bal => c(b+b)}} ,
         c => acctLook(pn){balLook(_){c}})(End)
    }
}


/**
 * Perform phone number -> balance twice and
 * return sum of balance, sequentially.
 */
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

/**
 * As for TwoLineBalanceSequential,but perform phone number -> account number lookup only once
 */
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

/**
 * Two balance lookups, returning the sum
 */
object TwoLineBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  next = {var total = Bal(0);b:Bal => total += b;total}
       Flow.inject(next)(
         c => acctLook(pn){balLook(_){c}} ,
         c => acctLook(pn){balLook(_){c}})(End)
    }
}

/**
 * Two balance lookups, returning the sum.
 * Optimize by performing the account lookup only once.
 */
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

/**
 * Demonstrate how additional logic could be placed after the inject
 */
object TwoLineBalanceDoubledInline{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       val  sum = {var total = Bal(0);b:Bal => total += b;total}
       val next = {b:Bal => End(b + Bal(13F))}
       Flow.inject(sum)(
       c => acctLook(pn){balLook(_){b:Bal =>c(b+b)}} ,
       c => acctLook(pn){balLook(_){b:Bal =>c(b+b)}})(next)
    }

} 

/**
 * Polymorphic operations, using common trait
 */
object PrepaidAndBalance{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) = {
      val  next = {var list = new scala.collection.mutable.ListBuffer[BalanceLike];b:BalanceLike => list+=b;list toList}
      Flow.inject(next)(
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){a:Acct => ppLook(a){c}})(End)
    }
}

/**
 * Demonstrate conditional logic within the flow, testing
 * against the account number returned by the lookup.
 * The balance of gamma is returned whenever alpha is returned.
 */
object SingleLineBalanceOrEnd{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      acctLook(pn){_ match {
          case Acct("alpha")  => balLook(Acct("gamma"))(End)
          case _ @ a => balLook(a)(End)
       }
    }
  }
}

/**
 * Demonstrate usage of state and decomposed logic
 */
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

/**
 * Demonstrate usage of state and decomposed logic
 */
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

/**
 * Capture results in order of return from service
 */
object SingleLineBalanceAccummulate{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.collect[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}

/**
 * Capture results are ordered list
 */
object SingleLineBalanceOrdered{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.ordered[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}

/**
 * Return two results as a tuple
 */
object SingleLineBalanceTupled{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2[Acct,Bal](
        c => acctLook(pn){c} ,
        c => acctLook(pn){balLook(_){c}}) (End)
  }
}

/**
 * Capture two results as a tuple and return tuple as a string
 */
object SingleLineBalanceTupledString{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.tupled2[Acct,Bal](
        c => acctLook(pn){c} ,
        c => acctLook(pn){balLook(_){c}}) ({t:Tuple2[Acct,Bal] => End(t.toString)})
  }
}

/**
 * Return the first result received from any service
 */
object SingleLineBalanceFirst{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      Flow.first[Bal](
        c => acctLook(pn){balLook(_){c}} ,
        c => acctLook(pn){balLook(_){c}})(End) 
  }
}

/**
 * Demonstrate chaining of first
 */
object SingleLineBalanceFirstChained{
    def apply(pn:Num)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      first[Acct]( c => acctLook(pn){c}){a:Acct => first[Bal]{d=>balLook(a){d}} (End)}
  }
}

/**
 * Given an id, perform parallel lookup of both account numbers and balances.
 * Return the sum of all the balances.
 */
object ListBalance{
   def apply(i:Int)(implicit numLook:Lookup[Int,List[Num]], acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       numLook(i)(
          scatter(acctLook){
             scatter(balLook){
                 b:Traversable[Bal] => End(
                   b.foldRight(Bal(0F))(_ + _) 
                 )
              }
          }
       )
   }
} 

/**
 * Given an id, perform lookup of phone numbers, followed by parallel lookup of account numbers.
 * Return a List containing the account numbers.
 */
object ParallelIdentity{
   def apply(i:Int)(implicit numLook:Lookup[Int,List[Num]], acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       numLook(i)(
          parallel[Num,Acct]{c => {n:Num => acctLook(n){c}}}{
             End
          }
       )
   }
} 

/**
 * Given an id, perform lookup of phone numbers, followed by parallel lookup of balances (via account number lookup).
 * Return a List containing the balances.
 */
object ParallelBalance{
   def apply(i:Int)(implicit numLook:Lookup[Int,List[Num]], acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
       numLook(i)(
          parallel[Num,Bal]
             {
                c => {n:Num => acctLook(n){balLook(_){c}}}
             } 
             {
               End
             }
       )
   }
} 

/**
 * Perform a parallel lookup for the phone numbers,
 * capturing the results in individual variables,
 * and returning the result as a tuple
 */
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

/**
 * Perform a parallel lookup for
 * the two telephone numbers, returning a list containing
 * the account numbers.
 */
object SplitGather{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      /*  must be a val (and not a def) to capture the required state
       *  Expected quantity (2) must be specified since code has
       * no way to determine the count in general. This also allows
       * for an N-of-M implementation.
       */
      val term = gather[Acct](2)(End)
      split (
         acctLook(Num("124-555-1234")){term},
         acctLook(Num("333-555-1234")){term}
      )
    }
}

/**
 * Return first Account number received from the service for either 
 * of the two telephone numbers, performing a parallel lookup. 
 * The unit test has fixed ordering so the result never varies.
 * In reality, the result would depend on the relative timing
 * of the referenced service.
 */
object SplitAny{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
    
      val term = any[Acct](End)
      split (
         acctLook(Num("124-555-1234")){term},
         acctLook(Num("333-555-1234")){term}
      )
    }
}

/**
 * Perform different operations depending on argument
 * one -> Account number for 124-555-1234
 * two -> Balance for 333-555-1234
 */
object Conditional{
    def apply(s:String)(implicit acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal]) = {
      s match {
         case "one" => acctLook(Num("124-555-1234")){End}
         case "two" => acctLook(Num("333-555-1234")){balLook(_)(End)}
         case _ => End("unmatched")
      }
    }
}
