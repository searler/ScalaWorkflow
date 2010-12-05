package workflow

class FlowActor[A](flow:A=>RPF) extends scala.actors.Actor{
  var pfs:List[RPF] = Nil
  var originator:scala.actors.OutputChannel[Any] = _
  def act(){
     loop {
     react {
       case (ci:CI,r:Any) => process(ci,r) //process incoming service response
       case arg:A => originator = sender;pfs = flow(arg) :: pfs; //start
       case _ @ x=> println("unexpected",x)
     }
   }
  }

  def process(ci:CI,in:Any){
     val out = (pfs.filter(pf=>pf.isDefinedAt(ci))).head.apply(ci)(in)
     out match {
        case r:Result[_] => originator ! Extract(r); exit //all done
        case Done => //ignored //intermediate process ended
        case r:RPF => pfs = r::pfs //another service call
        case _ @ x =>  println("unexpected",x)
     }
  }
}