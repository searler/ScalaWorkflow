package workflow

import org.specs._

object ServicesTest extends Specification{


 import Services._
import Flow._


object Extract{
   def apply[A](r:RPF):A = (r.asInstanceOf[Result[A]]).value
}


 "account lookup" in {
    val cb = acctLook(Num("124-555-1234"))(End)
    
    val a = Acct("alpha")
    val res = cb(CI(1))(a)
    val r:Acct = Extract(res)
    a must beEqualTo(r)
 } 

}