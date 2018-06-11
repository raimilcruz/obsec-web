package scala.ObSecG

import ObSecG.Ast._

trait ElementServiceBaseSpec{
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = LabelVarImpl(x)
  def ST(t1:TypeG,t2:LabelG)=STypeG(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationG]) = ObjectType(x,methods)
  def MD(x:String,mt:MTypeG)=MethodDeclarationG(x,mt)
}

trait BaseSpec {

  def tI(x:String)= TypeIdentifier(x)
  def bL(x:String,lower :TypeAnnotation,upper:TypeAnnotation)= BoundedLabelVariableDeclaration(x,lower,upper)
  def uL(x:String,upper:TypeAnnotation)= SubLabelVariableDeclaration(x,upper)
  def stA(t1:TypeAnnotation,t2:TypeAnnotation)=AnnotatedFacetedType(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationNode]) =ObjectTypeNode(x,methods)
  def MD(x:String,mt:MethodTypeNode):MethodDeclarationNode=MethodDeclarationNode(x,mt)
}
