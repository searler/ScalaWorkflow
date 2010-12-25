package workflow

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

  "ListBalance" in {
    ch(ListBalance(_:Int),123,Bal(125.5F))
  }

}