package scala.ObSecG

import ObSecG.Ast._

trait BaseSpec {
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = GenericTypeVar(x)
  def ST(t1:TypeG,t2:TypeG)=STypeG(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationG]) =ObjectType(x,methods)
  def MD(x:String,mt:MTypeG):MethodDeclarationG=MethodDeclarationG(x,mt)
}
