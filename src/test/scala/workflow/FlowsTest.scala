
package workflow


import org.specs._

object FlowsTest extends Specification {

  

  "oneLineBalance" in {
Services.requests
   val cb1 = SingleLineBalance(Num("124-555-1234"))
   val cb2 = cb1(1)(Acct("alpha"))
   cb2(2)(Bal(124.5F))
   Bal(124.5F) must beEqualTo(Services.BalanceReturn.get)
   List(Num("124-555-1234"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 
/*
 "twoLineBalance" in {
Services.requests
   val cb1 = TwoLineBalance(Num("124-555-1234"))
  /* val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
   val bal1:Option[Bal] = cb2(3)(Bal(124.5F))
   val bal2:Option[Bal]  = cb3(4)(Bal(124.5F))
   None must beEqualTo(bal1)
   Some(Bal(249.0F)) must beEqualTo(bal2)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) */
  } 
/*
"PrepaidAndBalance" in {
Services.requests
  import Services._
   val cb1 = PrepaidAndBalance(Num("124-555-1234"))(AccountLookup,BalanceLookup,PrepaidLookup)
   val cb2 = cb1(1)(Acct("alpha"))
   val cb3 = cb1(2)(Acct("alpha"))
   
    val bal1:Option[Bal] = cb2(3)(Bal(124.5F))
  val bal2:Option[Prepaid]  = cb3(4)(Prepaid(124.5F))
   None must beEqualTo(bal1)
   Some(Bal(249.0F)) must beEqualTo(bal2)
  List(Num("124-555-1234"),Num("124-555-1234"),Acct("alpha"),Acct("alpha")) must beEqualTo(Services.requests) 
  } 

*/
 */

}