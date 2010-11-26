package workflow

case class PartialFunctionCollection(list:Seq[RPF]) extends RPF{
  def isDefinedAt(x: Int): Boolean = list.exists(pf=>pf.isDefinedAt(x))
  def apply(x:Int):Any=>RPF = try {
                          (list.filter(pf=>pf.isDefinedAt(x))).head.apply(x)
                      }catch{
                        case _ => throw new IllegalArgumentException(x.toString)
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
