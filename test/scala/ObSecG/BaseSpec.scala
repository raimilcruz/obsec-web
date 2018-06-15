package scala.ObSecG

import ObSecG.Ast._

trait ElementServiceBaseSpec{
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = LabelVar(x)
  def ST(t1:TypeG,t2:LabelG)=STypeG(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationG]) = ObjectType(x,methods)
  def MD(x:String,mt:MTypeG)=MethodDeclarationG(x,mt)
  def MT(typeVars: List[BoundedLabelVar],
         domain: List[STypeG],
         codomain: STypeG): MTypeG = MTypeG(typeVars,domain,codomain)
  def BL(x:String,lower :LabelG,upper:LabelG)= BoundedLabelVar(x,lower,upper)
}

trait BaseSpec {
  implicit def stringToVar(x:String):VariableNode = VariableNode(x)
  implicit def listToNodeList[T <: ObSecGAstNode](list:List[T]):AstNodeList[T] = AstNodeList(list)
  def tI(x:String)= TypeIdentifier(x)
  def LL = LowLabelNode
  def bL(x:String,lower :TypeAnnotation,upper:TypeAnnotation)= BoundedLabelVariableDeclaration(x,lower,upper)
  def uL(x:String,upper:TypeAnnotation)= SubLabelVariableDeclaration(x,upper)
  def superL(x:String,lower:TypeAnnotation)= SuperLabelVariableDeclaration(x,lower)
  def stA(t1:TypeAnnotation,t2:TypeAnnotation)=AnnotatedFacetedType(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationNode]) =ObjectTypeNode(x,methods)
  def UL(left:TypeAnnotation,right: TypeAnnotation) = UnionTypeAnnotation(left,right)
  def MT(typeVars: List[LabelVariableDeclarationNode],
         domain: List[AnnotatedFacetedType],
         codomain: AnnotatedFacetedType): MethodTypeNode = MethodTypeNode(typeVars,domain,codomain)
  def NoRecOT(methods:List[MethodDeclarationNode]) =NoRecursiveObjectTypeNode(methods)
  def MD(x:String,mt:MethodTypeNode):MethodDeclarationNode=MethodDeclarationNode(SimpleIdentifier(x),mt)
  def MDef(x:String,args: List[String],body:ObSecGAstExprNode):MethodDefinitionNode=
    MethodDefinitionNode(SimpleIdentifier(x),AstNodeList(args.map(s=> SimpleIdentifier(s))),body)
  def MI(e1: ObSecGAstExprNode,
         types:List[TypeAnnotation],
         args: List[ObSecGAstExprNode],
         method: String):MethodInvocationNode=MethodInvocationNode(e1,types,args,SimpleIdentifier(method))

}
