package workflow

class FlowActor[A](flow:A=>RPF) extends scala.actors.Actor{
  var pfs:List[RPF] = Nil
  var originator:scala.actors.OutputChannel[Any] = _
  def act(){
     loop {
     react {
       case (ci:CI,r:Any) => process(ci,r) //process incoming service response
       case arg:A => { 
                        originator = sender
                        flow(arg) match {
                              case rc:RPFCollection => pfs = rc toList
                              case _ @ x => pfs = x :: Nil 
                            }
                     }
       case _ @ x=> println("unexpected",x)
     }
   }
  }

  def process(ci:CI,in:Any){
     val (matched,remaining)  = pfs.partition(pf=>pf.isDefinedAt(ci))
     pfs  = remaining
     matched.foreach{_.apply(ci)(in) match {
        case r:Result[_] => originator ! Extract(r); exit //all done
        case Done => //ignored intermediate process ended
        case rc:RPFCollection => pfs = rc.toList ::: pfs
        case r:RPF => pfs = r::pfs //another service call
        case _ @ x =>  println("unexpected",x)
       }
     }
  }
}

object FlowActor {
  def apply[A](flow:A=>RPF,initial:A) = {
      val a = new FlowActor(flow)
      a.start
      a ! initial
  }
}