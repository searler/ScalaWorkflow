package workflow

import org.specs._

object ServicesTest extends Specification{


 import Services._
 import EndObject._

 "account lookup" in {
    Services.requests
    val cb = AccountLookup(Num("123-555-1234"))(End)
    
    val a = Acct("alpha")
    val res = cb(1)(a)
    val r:Acct = Result(res)
    a must beEqualTo(r)

   val ra:Any = Result(res)
    a must beEqualTo(ra)

   

   List(Num("123-555-1234")) must beEqualTo(requests)
 } 

}