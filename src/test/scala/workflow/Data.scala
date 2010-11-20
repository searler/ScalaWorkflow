package workflow

case class Num(s:String)

case class Acct(s:String)

case class Bal(v:Float){
    def +(o:Bal) = Bal(v+o.v)
}