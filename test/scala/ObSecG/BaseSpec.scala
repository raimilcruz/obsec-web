package scala.ObSecG

import ObSecG.Ast._

trait ElementServiceBaseSpec{
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = LabelVar(x)
  def ST(t1:TypeG,t2:LabelG)=STypeG(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationG]) = ObjectType(x,methods)
  def MD(x:String,mt:MTypeG)=MethodDeclarationG(x,mt)
}

trait BaseSpec {
  implicit def stringToVar(x:String):VariableNode = VariableNode(x)
  def tI(x:String)= TypeIdentifier(x)
  def bL(x:String,lower :TypeAnnotation,upper:TypeAnnotation)= BoundedLabelVariableDeclaration(x,lower,upper)
  def uL(x:String,upper:TypeAnnotation)= SubLabelVariableDeclaration(x,upper)
  def superL(x:String,lower:TypeAnnotation)= SuperLabelVariableDeclaration(x,lower)
  def stA(t1:TypeAnnotation,t2:TypeAnnotation)=AnnotatedFacetedType(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationNode]) =ObjectTypeNode(x,methods)
  def MT(typeVars: List[LabelVariableDeclarationNode],
         domain: List[AnnotatedFacetedType],
         codomain: AnnotatedFacetedType): MethodTypeNode = MethodTypeNode(typeVars,domain,codomain)
  def NoRecOT(methods:List[MethodDeclarationNode]) =NoRecursiveObjectTypeNode(methods)
  def MD(x:String,mt:MethodTypeNode):MethodDeclarationNode=MethodDeclarationNode(x,mt)
  def MI(e1: ObSecGAstExprNode,
         types:List[TypeAnnotation],
         args: List[ObSecGAstExprNode],
         method: String):MethodInvocationNode=MethodInvocationNode(e1,types,args,method)

}
