package workflow

case class RPFCollection(list:Traversable[RPF]) extends RPF{
  def isDefinedAt(ci: CI): Boolean = list.exists(pf=>pf.isDefinedAt(ci))
  def apply(ci:CI):Any=>RPF = try {
                          (list.filter(pf=>pf.isDefinedAt(ci))).head.apply(ci)
                      }catch{
                        case _ => throw new IllegalArgumentException(ci toString)
                      }
  override def toString = list.toString
}

object RPFCollection{
    def concat[C,D](f:C=>D, result:Function1[D,RPF])(fa:(C=>RPF)=>RPF*) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1;if(count==0)result(f(arg));  else {f(arg);Done}}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }
}
