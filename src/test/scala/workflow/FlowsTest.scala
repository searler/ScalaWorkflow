package workflow


import org.specs._

object FlowsTest extends Specification {

  val CI ="ignored"

  "linearBalance" in {

   val cb1 = SingleLineBalance(Num("124-555-1234"))
   val cb2 = cb1(CI)(Acct("alpha"))
   val cb3 = cb2(CI)(Bal(124.5F))
   Bal(124.5F) must beEqualTo(Bal(124.5F))
   

  } 

}