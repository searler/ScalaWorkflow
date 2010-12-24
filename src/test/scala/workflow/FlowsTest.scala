package workflow

import org.specs._

object FlowsTest extends Specification {

 import Services._
 import scala.actors.Actor._

 private def check(flow:Num=>RPF,expected:Bal=Bal(124.5F)) {
    FlowActor(flow,Num("124-555-1234"))
    receiveWithin(1000L){
       case b:Bal => b  must beEqualTo(expected)
       case scala.actors.TIMEOUT => fail("timeout")
       case _ @ x=> fail(x toString)
      }
 }

  "oneLineBalanceActor" in {
   import Services.accountServer
   import Services.balanceServer
   import Services.LookupActor
 
   check(SingleLineBalanceBuilder(new LookupActor(accountServer), new LookupActor(balanceServer)))     
  } 

"oneLineBalanceSelf" in {
   check(SingleLineBalanceBuilder(new LookupSelf(acctMap), new LookupSelf(balMap)))
  } 

"twoLineBalanceActorSelf" in {
  import Services.accountServer
   import Services.balanceServer
   import Services.LookupActor
 
   check(TwoLineBalance(_:Num)(new LookupActor(accountServer), new LookupActor(balanceServer)),Bal(249F))  
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
/*
"twoLineBalanceSequentialPipeline" in {

 
   val cb1 = TwoLineBalanceSequentialPipeline(_)
   val cb2 = cb1(CI("1"))(Acct("alpha"))
   val cb3 =  cb2(CI("2"))(Bal(124.5F))
   val cb4=  cb3(CI("3"))(Bal(124.5F))

   List(_,Acct("alpha"),Acct("alpha")) must beEqualTo() 
   val res:Bal = Extract( cb4)
 
  } 
*/

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

/*
"PrepaidAndBalance" in {

  
   val cb1 = PrepaidAndBalance(_)
   val cb2 = cb1(CI("1"))(Acct("alpha"))
   val cb3 = cb1(CI("2"))(Acct("alpha"))
   cb3(CI("4"))(PP(24.5F)) 
    val res:List[BalanceLike] = Extract(cb2(CI("3"))(Bal(324.5F)))
  
   
   List( PP(24.5F),Bal(324.5F)) must beEqualTo(res)
  List(_,_,Acct("alpha"),Acct("alpha")) must beEqualTo() 
  } 


*/
"exclusiveSplitJoinVar" in {
   check(exclusiveSplitJoinVar(_))
  } 

"exclusiveSplitJoin" in { 
   check( exclusiveSplitJoin(_),Bal(249.0F) )
  } 

  "SingleLineBalanceOr" in { 
   check( SingleLineBalanceOr(_))
  } 

/*
  "SingleLineBalanceAccummulate" in {

 
   val cb1 = SingleLineBalanceAccummulate(_)
  val cb2 = cb1(CI("1"))(Acct("alpha"))
   val cb3 = cb1(CI("2"))(Acct("alpha"))
    cb2(CI("4"))(Bal(124.5F))
    val res:List[Bal] = Extract(cb3(CI("3"))(Bal(24.5F)))
    
 
  List(Bal(124.5F), Bal(24.5F)) must beEqualTo(res)
  List(_,_,Acct("alpha"),Acct("alpha")) must beEqualTo() 
  
  } 


  "SingleLineBalanceOrdered" in {

 
   val cb = SingleLineBalanceOrdered(_)
   val cb2 = cb(CI("2"))(Acct("alpha"))
   val cb2b= cb2(CI("x"))(Bal(124.5F))
  val cb1 = cb(CI("1"))(Acct("beta"))
  val cb1b = cb1(CI("x"))(Bal(24.5F))
 
    val res:List[(Int,Bal)] = Extract(cb1b)
    
 
  List(Bal(24.5F), Bal(124.5F)) must beEqualTo(res)
  List(_,_,Acct("alpha"),Acct("beta")) must beEqualTo() 
  
  } 

*/
/*
  "SingleLineBalanceTupled" in {

 
   val cb = SingleLineBalanceTupled(_)
   val cb2 = cb(CI("2"))(Acct("alpha"))
   val cb2b= cb2(CI("x"))(Bal(124.5F))
  val cb1 = cb(CI("1"))(Acct("beta"))

    val res:(Acct,Bal) = Extract(cb1)
    
 
  (Acct("beta"), Bal(124.5F)) must beEqualTo(res)
  List(_,_,Acct("alpha")) must beEqualTo() 
  
  } 

*/
  "SingleLineBalanceFirst" in {
  check( SingleLineBalanceFirst(_))
  } 

}