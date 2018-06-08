package scala.ObSecG

import ObSecG.Ast._

trait BaseSpec {
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = LabelVarImpl(x)
  def ST(t1:TypeG,t2:LabelG)=STypeG(t1,t2)



  def tI(x:String)= TypeIdentifier(x)
  def bL(x:String,lower :TypeAnnotation,upper:TypeAnnotation)= BoundedLabelVariableDeclaration(x,lower,upper)
  def uL(x:String,upper:TypeAnnotation)= SubLabelVariableDeclaration(x,upper)
  def stA(t1:TypeAnnotation,t2:TypeAnnotation)=AnnotatedFacetedType(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationNode]) =ObjectTypeNode(x,methods)
  def MD(x:String,mt:MethodTypeNode):MethodDeclarationNode=MethodDeclarationNode(x,mt)
}
