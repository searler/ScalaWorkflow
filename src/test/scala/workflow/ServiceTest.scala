package workflow

import org.specs._

object ServicesTest extends Specification{


 import Services._


object Extract{
   def apply[A](r:RPF):A = (r.asInstanceOf[Result[A]]).value
}

/*
 "account lookup" in {
    Services.requests
    val cb = AccountLookup(Num("123-555-1234"))(End)
    
    val a = Acct("alpha")
    val res = cb(CI("1"))(a)
    val r:Acct = Extract(res)
    a must beEqualTo(r)

   val ra:Any = Extract(res)
    a must beEqualTo(ra)

   

   List(Num("123-555-1234")) must beEqualTo(requests)
 } 
*/
}