package workflow

import org.specs._

object ServicesTest extends Specification{


 import Services._

 "account lookup" in {
    Services.requests
    val cb = AccountLookup(Num("123-555-1234")){a:Acct=>a}
    
    val a = Acct("alpha")
    val r:Acct = cb(1)(a)

    a must beEqualTo(r)

   List(Num("123-555-1234")) must beEqualTo(requests)
 } 

}