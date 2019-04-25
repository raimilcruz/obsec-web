package scala.EObSec

import Common.AstNode
import ObSecE.Ast._


trait ElementServiceBaseSpec{
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  def GV(x:String) = LabelVar(x)
  def ST(t1:TypeE,t2:LabelE)=FTypeE(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationE]) = ObjectType(x,methods)
  def MD(x:String,mt:MTypeE)=MethodDeclarationE(x,mt)
  def MT(domain: List[STypeE],
         codomain: STypeE): MTypeE = MTypeE(domain,codomain)
}

trait BaseSpec {
  implicit def stringToVar(x:String):VariableNode = VariableNode(x)
  implicit def listToNodeList[T <: EObSecNode](list:List[T]):AstNodeList[T] = AstNodeList(list)
  def tI(x:String)= TypeIdentifier(x)
  def LL = LowLabelNode

  def stA(t1:TypeAnnotation,t2:TypeAnnotation)=AnnotatedSubtypingFacetedType(t1,t2)
  def OT(x:String,methods:List[MethodDeclarationNode]) =ObjectTypeNode(x,methods)

  def MT(typeVars: List[ExistentialVariableDeclarationNode],
         domain: List[AnnotatedFacetedType],
         codomain: AnnotatedFacetedType): MethodTypeNode = MethodTypeNode(domain,codomain)
  def NoRecOT(methods:List[MethodDeclarationNode]) =NoRecursiveObjectTypeNode(methods)
  def MD(x:String,mt:MethodTypeNode):MethodDeclarationNode=MethodDeclarationNode(SimpleIdentifier(x),mt)
  def MDef(x:String,args: List[String],body:EObSecNode):MethodDefinitionNode=
    MethodDefinitionNode(SimpleIdentifier(x),AstNodeList(args.map(s=> SimpleIdentifier(s))),body)
  def MI(e1: EObSecNode,
         args: List[EObSecNode],
         method: String):MethodInvocationNode=MethodInvocationNode(e1,args,SimpleIdentifier(method))

}
