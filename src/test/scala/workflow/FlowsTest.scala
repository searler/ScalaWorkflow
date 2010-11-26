
package workflow


import org.specs._

object FlowsTest extends Specification {

  

  "oneLineBalance" in {
Services.requests
   val cb1 = SingleLineBalance(Num("124-555-1234"))
   val cb2 = cb1(1333)(Acct("alpha")) //#############
   cb2(2)(Bal(124.5F))
   Bal(124.5F) must beEqualTo(Services.BalanceReturn.get)
   List(Num("124-555-1234"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 

 "twoLineBalance" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalance(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
    cb2(3)(Bal(124.5F))
     cb3(4)(Bal(124.5F))
    
 
  Bal(249.0F) must beEqualTo(Services.BalanceReturn.get)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 

"twoLineBalanceDoubled" in {
Services.requests
 import Services._
   val cb1 = TwoLineBalanceDoubled(Num("124-555-1234"))

   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
    cb2(3)(Bal(124.5F))
     cb3(4)(Bal(124.5F))
    
 
  Bal(498.0F) must beEqualTo(Services.BalanceReturn.get)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 

"PrepaidAndBalance" in {
Services.requests
  import Services._
   val cb1 = PrepaidAndBalance(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
   
   cb2(3)(Bal(124.5F))
   cb3(4)(PP(124.5F))
   
   List(Bal(124.5F), PP(124.5F)) must beEqualTo(PrepaidReturn.get)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 


 

}