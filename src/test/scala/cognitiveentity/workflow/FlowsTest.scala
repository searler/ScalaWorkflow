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

import org.specs2.mutable._

/**
 * Collections of tests of Flows, illustrating how they are used
 */
abstract class FlowsTest(implicit numLook:Lookup[Int,List[Num]],acctLook:Lookup[Num,Acct],  balLook:Lookup[Acct,Bal], ppLook:Lookup[Acct,PP]) extends Specification {

sequential

 /**
  * Common test code for a flow that accepts an A and
  * uses Matcher
  */ 
  protected def chMatch[A,R](flow:A=>RPF,n:A,m:org.specs2.matcher.Matcher[R]): org.specs2.execute.Result


 /**
  * Common test code for a flow that accepts an A and
  * returns an R
  */ 
  protected def ch[A,R](flow:A=>RPF,n:A,expected:R) = chMatch(flow,n,new org.specs2.matcher.BeEqualTo(expected))

 /**
  * Test a flow that takes Num("124-555-1234") and returns an R
  */
  protected def chk[R](flow:Num=>RPF,expected:R) =ch(flow,Num("124-555-1234"),expected)

 /**
  * Test a flow that takes a Num and returns a Bal, with a default
  * of Bal(124.5F)
  */
 protected def check(flow:Num=>RPF,expected:Bal=Bal(124.5F)) = chk(flow,expected)


"oneLineBalanceOrEnd" in { 
   check( SingleLineBalanceOrEnd(_),Bal(11F))
  } 



  "oneLineBalance" in {
   check(SingleLineBalance(_))
  } 


"oneLineBalanceAsPartial" in {
  check( SingleLineBalanceAsPartial(_))
 } 


 "oneLineBalanceAsTwo" in { 
   check( SingleLineBalanceAsTwo(_))
  } 

"oneLineBalanceAsTwoInitial" in { 
   check( SingleLineBalanceAsTwoInitial(_,Bal(1000F)),Bal(1124.5F))
   check( SingleLineBalanceAsTwoInitial(_,Bal(500F)),Bal(624.5F))
  } 

 "twoLineBalance" in {
   check( TwoLineBalance(_),Bal(249F))
  } 

"twoLineBalanceSequential" in {
   check(TwoLineBalanceSequential(_),Bal(249F))
  } 

"twoLineBalanceSequentialOptimized" in {
   check(TwoLineBalanceSequentialOptimized(_),Bal(249F))
  } 

"twoLineBalanceVarying" in {
  check(TwoLineBalanceVarying(_),Bal(373.5F) )
  } 

"twoLineBalanceSumVar" in {
   check(TwoLineBalanceSumVar(_),Bal(249.0F))
  } 

"twoLineBalanceSumVarInline" in { 
   check(TwoLineBalanceSumVarInline(_),Bal(249F))
  } 

"twoLineBalanceWithConstantSumVarInline" in { 
   check(TwoLineBalanceWithConstantSumVarInline(_),Bal(135.5F))
  } 

"twoLineBalanceEfficient" in { 
   check(TwoLineBalanceEfficient(_),Bal(249.0F))
  } 

"twoLineBalanceDoubled" in { 
  check(TwoLineBalanceDoubled(_),Bal(498.0F))
  } 

"twoLineBalanceDoubledInline" in { 
  check( TwoLineBalanceDoubledInline(_),Bal(511F) )
  } 

"PrepaidAndBalance" in {  
   chk(PrepaidAndBalance(_),(List(Bal(124.5F), PP(124.5F))))
  } 

"exclusiveSplitJoinVar" in {
   check(exclusiveSplitJoinVar(_),Bal(11F))
  } 

"exclusiveSplitJoinVarBeta" in {
   //Num type must be explicitly specified
   ch(exclusiveSplitJoinVar(_:Num),Num("333-555-1234"),Bal(1F))
  } 

"exclusiveSplitJoin" in { 
   check( exclusiveSplitJoin(_),Bal(249.0F) )
  } 

  "SingleLineBalanceOr" in { 
   check( SingleLineBalanceOr(_))
  } 

  "SingleLineBalanceAccummulate" in {
   chk( SingleLineBalanceAccummulate(_), List(Bal(124.5F), Bal(124.5F)) )
  } 

  "SingleLineBalanceOrdered" in { 
   chk(SingleLineBalanceOrdered(_), List(Bal(124.5F), Bal(124.5F)))
  } 

  "SingleLineBalanceTupled" in {
   chk(SingleLineBalanceTupled(_),(Acct("alpha"), Bal(124.5F)))
  } 

"SingleLineBalanceTupledString" in {
   chk(SingleLineBalanceTupledString(_),"""(Acct(alpha),Bal(124.5))""")
  } 

 "SingleLineBalanceTripled" in {
   chk(SingleLineBalanceTripled(_),(Acct("alpha"), Bal(124.5F),"Bal(124.5)"))
  } 

  "SingleLineBalanceFirst" in {
  check( SingleLineBalanceFirst(_))
  } 

"SingleLineBalanceFirstChained" in {
  check( SingleLineBalanceFirstChained(_))
  } 

  "ListBalance" in {
    ch(ListBalance(_:Int),123,Bal(125.5F))
  }

  "ParallelIdentity" in {
    chMatch(ParallelIdentity(_:Int),123,beEqualTo(List(Acct("alpha"), Acct("beta"))) or beEqualTo(List(Acct("beta"), Acct("alpha"))))
  }

 "ParallelBalance" in {
    ch(ParallelBalance(_:Int),123,List(Bal(124.5F),Bal(1F)))
  }


    "SplitJoin" in {
    ch(SplitJoin(_:String),"xxx",(Acct("alpha"),Acct("beta")))
  }

  "SplitGather" in {
    chMatch(SplitGather(_:String),"xxx",beEqualTo(List(Acct("alpha"), Acct("beta"))) or beEqualTo(List(Acct("beta"), Acct("alpha"))))
  }

"SplitAny" in {
    chMatch(SplitAny(_:String),"xxx",beEqualTo(Acct("alpha")) or beEqualTo(Acct("beta")) )
  }

 "Conditional one" in {
    ch(Conditional(_:String),"one",Acct("alpha"))
  }

 "Conditional two" in {
    ch(Conditional(_:String),"two",Bal(1F))
  }

"Conditional other" in {
    ch(Conditional(_:String),"xx","unmatched")
  }

"Empty RPFCollection" in {
  import Flow._
  val col = split()
  false must beEqualTo(col.isDefinedAt(CI(12)))
}

"Singleton RPFCollection" in {
  import Flow._
  val r1:RPF =  new RPF{
     def isDefinedAt(ci:CI) = ci.id==12
     def apply(ci:CI)= {case _ => Done}
  }
  val col = split(r1)
  true must beEqualTo(col.isDefinedAt(CI(12)))
  false must beEqualTo(col.isDefinedAt(CI(13)))
}

"Pair RPFCollection" in {
  import Flow._
  val r1:RPF =  new RPF{
     def isDefinedAt(ci:CI) = ci.id==12
     def apply(ci:CI)= {case c:CI => Result(ci.id *2)}
  }
 val r2:RPF =  new RPF{
     def isDefinedAt(ci:CI) = ci.id==13
     def apply(ci:CI)= {case c:CI => Result(ci.id * 3)}
  }
  val col = split(r1,r2)
  true must beEqualTo(col.isDefinedAt(CI(12)))
  true must beEqualTo(col.isDefinedAt(CI(13)))
  false must beEqualTo(col.isDefinedAt(CI(14)))
  col(CI(12))(CI(12))  match {
    case r:Result[_] => 24 must beEqualTo(r.value)
  }
 col(CI(13))(CI(13))  match {
    case r:Result[_] => 39 must beEqualTo(r.value)
  }
  
 

}



}
