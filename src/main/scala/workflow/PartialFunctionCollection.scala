package workflow

case class PartialFunctionCollection(list:Seq[RPF]) extends RPF{
  def isDefinedAt(ci: CI): Boolean = list.exists(pf=>pf.isDefinedAt(ci))
  def apply(ci:CI):Any=>RPF = try {
                          (list.filter(pf=>pf.isDefinedAt(ci))).head.apply(ci)
                      }catch{
                        case _ => throw new IllegalArgumentException(ci toString)
                      }
  override def toString = list.toString
}

object PartialFunctionCollection{
    def concat[C,D](f:C=>D, result:Function1[D,RPF])(fa:(C=>RPF)=>RPF*) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1;if(count==0)result(f(arg));  else {f(arg);Done}}
     val prepped:Seq[RPF] = fa.map(pf=>pf(counter _))
     new PartialFunctionCollection(prepped)
   }
}
