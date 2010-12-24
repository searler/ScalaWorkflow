package workflow

case class RPFCollection(list:Traversable[RPF]) extends RPF{
  def isDefinedAt(ci: CI): Boolean = list.exists(_.isDefinedAt(ci))
  def apply(ci:CI):Any=>RPF = try {
                          (list.filter(_.isDefinedAt(ci))).head.apply(ci)
                      }catch{
                        case _ => throw new IllegalArgumentException(ci toString)
                      }
  override def toString = "RPFC:"+list.toString
  def toList = list toList
}

object RPFCollection{
    def concat[C,D](f:C=>D, result:Function1[D,RPF])(fa:(C=>RPF)=>RPF*) = {
     var count = fa size
     def counter(arg:C):RPF = {count-=1;if(count==0)result(f(arg));  else {f(arg);Done}}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

   def first[C](result:Function1[C,RPF])(fa:(C=>RPF)=>RPF*) = {
     var found = false
     def counter(arg:C):RPF = {if(found) {Done}; else {found = true;result(arg)}}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

   // order of arrival
    def accummulate[C](result:Function1[List[C],RPF])(fa:(C=>RPF)=>RPF*) = {
     val buffer = new scala.collection.mutable.ListBuffer[C]() 
     def counter(arg:C):RPF = {buffer += arg; if(buffer.size == fa.size)result(buffer toList) else Done}
     new RPFCollection( fa.map(pf=>pf(counter _)))
   }

  def ordered[C](result:Function1[List[C],RPF])(fa:(C=>RPF)=>RPF*) = {
      import scala.collection.immutable._
     val buffer = new scala.collection.mutable.ListBuffer[(Int,C)]() 
     def counter(index:Int)(arg:C):RPF = {buffer += index->arg; if(buffer.size == fa.size)result(SortedMap(buffer:_*).values.toList) else Done}
     new RPFCollection( fa.zipWithIndex.map(p=>p._1(counter(p._2) _)))
   }
  
  def tupled[A,B](result:Function1[(A,B),RPF])(fa:(A=>RPF)=>RPF,fb:(B=>RPF)=>RPF) = {
     var a:Option[A] = None
     var b:Option[B] = None
     def processA(arg:A):RPF = {a= Some(arg);if(b==None)Done else result(a.get->b.get);}
     def processB(arg:B):RPF = {b= Some(arg);if(a==None)Done else result(a.get->b.get);}
     new RPFCollection(List(fa(processA),fb(processB)))
  } 
    
}
