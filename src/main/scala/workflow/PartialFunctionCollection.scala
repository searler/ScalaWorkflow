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

   def first[C](fa:(C=>RPF)=>RPF*)(result:Function1[C,RPF]) = {
     var found = false
     def counter(arg:C):RPF = {if(found) {Done}; else {found = true;result(arg)}}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

    def accummulate[C](result:Function1[List[C],RPF])(fa:(C=>RPF)=>RPF*) = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {buffer + arg; if(buffer.size == fa.size)result(buffer toList) else Done}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

}
