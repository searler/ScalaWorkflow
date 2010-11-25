package workflow

import org.specs._

object ServicesTest extends Specification{


 import Services._

 "account lookup" in {
    Services.requests
    val cb = AccountLookup(Num("123-555-1234"))(AccountReturn)
    
    val a = Acct("alpha")
    val r:RPF = cb(1)(a)

    a must beEqualTo(AccountReturn.get)

   List(Num("123-555-1234")) must beEqualTo(requests)
 } 

}