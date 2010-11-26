package workflow

case class Num(s:String)

case class Acct(s:String)

trait BalanceLike{
 def v:Float
}

case class Bal(v:Float) extends BalanceLike{
    def +(o:Bal) = Bal(v+o.v)
}

case class PP(v:Float) extends BalanceLike