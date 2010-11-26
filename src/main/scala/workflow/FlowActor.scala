package workflow

class FlowActor[A](flow:A=>RPF) extends scala.actors.Actor{
  var pfs:List[RPF] = Nil
  var originator:scala.actors.OutputChannel[Any] = _
  def act(){
     loop {
     react {
       case (ci:Int,r:Any) => process(ci,r) 
       case arg:A => originator = sender;pfs = flow(arg) :: pfs; println(pfs)
       case _ @ x=> println("unexpected",x)
     }
   }
  }

  def process(ci:Int,in:Any){
     val out = (pfs.filter(pf=>pf.isDefinedAt(ci))).head.apply(ci)(in)
     out match {
        case r:Result[_] => println(Result(r)); originator ! Result(r); exit
        case Done => //ignored
        case r:RPF => pfs = r::pfs
        case _ @ x =>  println("unexpected",x)
     }
     println(pfs)
  }
}