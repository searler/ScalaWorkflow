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

import org.specs._

object FlowsTest extends Specification {

 import Services._
 import scala.actors.Actor._

  private def ch[A,R](flow:A=>RPF,n:A,expected:R) {
    FlowActor(flow,n)
    receiveWithin(1000L){
       case b:R => b  must beEqualTo(expected)
       case scala.actors.TIMEOUT => fail("timeout")
       case _ @ x=> fail(x toString)
      }
 }

 private def chk[R](flow:Num=>RPF,expected:R) =ch(flow,Num("124-555-1234"),expected)

 private def check(flow:Num=>RPF,expected:Bal=Bal(124.5F)) = chk(flow,expected)

"oneLineBalanceSelf" in {
   check(SingleLineBalanceBuilder(new LookupSelf(acctMap), new LookupSelf(balMap)))
  } 

"oneLineBalanceSelfPartial" in {
   check(SingleLineBalance(_:Num)(new LookupSelf(acctMap), new LookupSelf(balMap)))
  } 

  "oneLineBalance" in {
   check(SingleLineBalance(_))
  } 

"oneLineBalanceAsPartial" in {
  check( SingleLineBalanceAsPartial(_))
 } 

"oneLineBalanceOrEnd" in { 
   check( SingleLineBalanceOrEnd(_))
  } 

 "oneLineBalanceAsTwo" in { 
   check( SingleLineBalanceAsTwo(_))
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

"twoLineBalanceEfficient" in { 
   check(TwoLineBalanceEfficient(_),Bal(249.0F))
  } 

"twoLineBalanceDoubled" in { 
  check(TwoLineBalanceDoubled(_),Bal(498.0F))
  } 

"twoLineBalanceDoubledInline" in { 
  check( TwoLineBalanceDoubledInline(_),Bal(498.0F) )
  } 

"PrepaidAndBalance" in {  
   chk(PrepaidAndBalance(_),(List(Bal(124.5F), PP(124.5F))))
  } 

"exclusiveSplitJoinVar" in {
   check(exclusiveSplitJoinVar(_),Bal(11F))
  } 

"exclusiveSplitJoinVarBeta" in {
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
    ch(ParallelIdentity(_:Int),123,List(Acct("alpha"), Acct("beta")))
  }

 "ParallelBalance" in {
    ch(ParallelBalance(_:Int),123,List(Bal(124.5F),Bal(1F)))
  }


    "SplitJoin" in {
    ch(SplitJoin(_:String),"xxx",(Acct("alpha"),Acct("beta")))
  }

  "SplitGather" in {
    ch(SplitGather(_:String),"xxx",List(Acct("alpha"), Acct("beta")))
  }

"SplitAny" in {
    ch(SplitAny(_:String),"xxx",Acct("alpha"))
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

}