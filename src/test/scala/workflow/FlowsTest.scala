
package workflow


import org.specs._

object FlowsTest extends Specification {




  "oneLineBalanceActor" in {
   import Services.accountServer
   import Services.balanceServer
   import Services.LookupActor
   import scala.actors.Actor._

  // val act = new FlowActor[Num]({n:Num => SingleLineBalance(n)(new LookupActor[Num,Acct](accountServer), new LookupActor[Acct,Bal](balanceServer))})
   val creator = new SingleLineBalanceBuilder()(new LookupActor[Num,Acct](accountServer), new LookupActor[Acct,Bal](balanceServer))
   val act = new FlowActor[Num](creator)
   act.start
   
      act ! Num("124-555-1234")
      receiveWithin(1000L){
       case b:Bal => b  must beEqualTo(Bal(124.5F))
       case scala.actors.TIMEOUT => fail("timeout")
       case _ @ x=> fail(x toString)
      }
   
    
   
   
  } 


  "oneLineBalance" in {
Services.requests
 import Services._
   val cb1 = SingleLineBalance(Num("124-555-1234"))
   List(Num("124-555-1234")) must beEqualTo(Services.requests)  
   val cb2 = cb1(1333)(Acct("alpha")) //#############
   List(Acct("alpha")) must beEqualTo(Services.requests) 
   val res:Bal = Extract(cb2(2)(Bal(124.5F)))
   Bal(124.5F) must beEqualTo(res)
  
  } 


 "oneLineBalanceAsTwo" in {
Services.requests
 import Services._
   val cb1 = SingleLineBalanceAsTwo(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val res:Bal =Extract(cb2(2)(Bal(124.5F)))
   Bal(124.5F) must beEqualTo(res)
   List(Num("124-555-1234"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 


 "twoLineBalance" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalance(Num("124-555-1234"))
   List(Num("124-555-1234"),Num("124-555-1234")) must beEqualTo(Services.requests) 
   val cb2 = cb1(1)(Acct("alpha"))
   List(Acct("alpha")) must beEqualTo(Services.requests) 
   val cb3 = cb1(2)(Acct("alpha"))
   List(Acct("alpha")) must beEqualTo(Services.requests) 
    cb2(3)(Bal(124.5F))
   val res:Bal = Extract( cb3(4)(Bal(124.5F)))
    
 
  Bal(249.0F) must beEqualTo(res)
  
  } 

"twoLineBalanceSequential" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceSequential(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 =  cb2(2)(Bal(124.5F))
   val cb4  = cb3(3)(Acct("alpha"))
   val cb5 =  cb4(4)(Bal(124.5F))

   List(Num("124-555-1234"),Acct("alpha"),Num("124-555-1234"),Acct("alpha")) must beEqualTo(Services.requests) 
   val res:Bal = Extract( cb5)
    
 
  Bal(249.0F) must beEqualTo(res)
 
  } 
/*
"twoLineBalanceSequentialPipeline" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceSequentialPipeline(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 =  cb2(2)(Bal(124.5F))
   val cb4=  cb3(3)(Bal(124.5F))

   List(Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
   val res:Bal = Extract( cb4)
 
  } 
*/

"twoLineBalanceSequentialOptimized" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceSequentialOptimized(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 =  cb2(2)(Bal(124.5F))
   val cb4=  cb3(3)(Bal(124.5F))

  
   val res:Bal = Extract( cb4)
    
 
  Bal(249.0F) must beEqualTo(res)
 List(Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
 
  } 

"twoLineBalanceVarying" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceVarying(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
    cb2(3)(Bal(124.5F))
   val res:Bal = Extract( cb3(4)(Bal(124.5F)))
    
 
  Bal(373.5F) must beEqualTo(res)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 

"twoLineBalanceSumVar" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceSumVar(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
    cb2(3)(Bal(124.5F))
   val res:Bal = Extract( cb3(4)(Bal(124.5F)))
    
 
  Bal(249.0F) must beEqualTo(res)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 




"twoLineBalanceEfficient" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceEfficient(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
    cb2(2)(Bal(124.5F))
    val res:Bal =  Extract(cb2(3)(Bal(124.5F)))
    
 
  Bal(249.0F) must beEqualTo(res)
  List(Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 



"twoLineBalanceDoubled" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceDoubled(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
    cb2(3)(Bal(124.5F))
    val res:Bal = Extract(cb3(4)(Bal(124.5F)))
    
 
  Bal(498.0F) must beEqualTo(res)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 


"PrepaidAndBalance" in {
Services.requests
  import Services._
   val cb1 = PrepaidAndBalance(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
   
   cb2(3)(Bal(124.5F))
   val res:List[BalanceLike] = Extract(cb3(4)(PP(124.5F)))
   
   List(Bal(124.5F), PP(124.5F)) must beEqualTo(res)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 


}