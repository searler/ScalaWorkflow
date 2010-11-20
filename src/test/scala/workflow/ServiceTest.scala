package workflow

import org.specs._

object ServicesTest extends Specification{

 val CI = "ignored"

 import Services._

 "account lookup" in {
    
    val cb = AccountLookup(Num("123-555-1234")){a:Acct=>a}
    
    val a = Acct("alpha")
    val r:Acct = cb(CI)(a)

    a must beEqualTo(r)

   List(Num("123-555-1234")) must beEqualTo(requests)
 } 

}