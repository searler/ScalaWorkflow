package workflow

import org.specs._

object PartialFunctionCollectionTest extends Specification{

  "single" in {
     val pf:PartialFunction[String,String] = {case "xxx" =>"yyy"}
     val pfc:PartialFunction[String,String] = new PartialFunctionCollection(List(pf))
     true must beEqualTo(pfc.isDefinedAt("xxx"))
     false must beEqualTo(pfc.isDefinedAt("not"))
     "yyy" must beEqualTo(pfc("xxx"))
   }

  "pair" in {
    val pf1:PartialFunction[String,String] = {case "xxx" =>"yyy"}
    val pf2:PartialFunction[String,String] = {case "aaa" =>"bbb"} 
     val pfc = new PartialFunctionCollection(List(pf1,pf2))
     true must beEqualTo(pfc.isDefinedAt("xxx"))
     true must beEqualTo(pfc.isDefinedAt("aaa"))
     false must beEqualTo(pfc.isDefinedAt("not"))
     "yyy" must beEqualTo(pfc("xxx"))
     "bbb" must beEqualTo(pfc("aaa"))
   }

  "fails" in {
     val pf:PartialFunction[String,String] = {case "xxx" =>"yyy"}
     val pfc:PartialFunction[String,String] = new PartialFunctionCollection(List(pf))
     false must beEqualTo(pfc.isDefinedAt("not"))
      pfc("not") must throwA(new IllegalArgumentException("not"))
  }

}