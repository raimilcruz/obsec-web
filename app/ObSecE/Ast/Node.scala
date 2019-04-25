package ObSecE.Ast

import Common.{AstNode}


trait EObSecNode extends AstNode{
  def children: List[EObSecNode]
}
case class SimpleIdentifier(name:String) extends EObSecNode {
  override def children: List[EObSecNode] = List()
}

case class VariableNode(name:String) extends EObSecNode {
  override def children: List[EObSecNode] = List()
}
case class ObjectDefinitionNode(selfName: String,
                                selfAscription: AnnotatedFacetedType,
                                methods: List[MethodDefinitionNode])
  extends EObSecNode {
  override def children: List[MethodDefinitionNode] = methods
}
case class MethodInvocationNode(e1: EObSecNode,
                                args: AstNodeList[EObSecNode],
                                method: SimpleIdentifier) extends EObSecNode {
  override def children: List[EObSecNode] = List(e1) ++ args.elems ++ List(method)
}
case class AstNodeList[+T <: EObSecNode](elems: List[T]) extends EObSecNode {
  if(elems.nonEmpty){
    setPos(elems.head.pos)
    setEndPos(elems.last.endPos)
  }
  override def children: List[EObSecNode] = elems
}

case class MethodDefinitionNode(methodName: SimpleIdentifier, args: AstNodeList[SimpleIdentifier], mBody: EObSecNode )
  extends EObSecNode {
  //override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
  override def children: List[EObSecNode] = List(mBody)
  def name:String = methodName.name
}

case class IfExpressionNode(cond:EObSecNode ,
                            thenPart:EObSecNode ,
                            elsePart:EObSecNode )
  extends EObSecNode  {
  override def children: List[EObSecNode] = List(cond,thenPart,elsePart)
}


case class IntLiteral(v: Int) extends EObSecNode  {
  override def children: List[EObSecNode] = List()
}
case class StringLiteral(v:String) extends EObSecNode  {
  override def children: List[EObSecNode] = List()
}
case class BooleanLiteral(v:Boolean) extends EObSecNode  {
  override def children: List[EObSecNode] = List()
}

case class ListLiteral(label: TypeAnnotation, elems: List[EObSecNode]) extends EObSecNode  {
  override def children: List[EObSecNode] = elems
}
case class ConsListOperatorNode(elem :EObSecNode ,list:EObSecNode) extends EObSecNode  {
  override def children: List[EObSecNode] = List(elem,list)
}

case class LetStarExpressionNode(declarations: List[DeclarationNode],body:EObSecNode) extends EObSecNode  {
  override def children: List[EObSecNode] = declarations ++ List(body)
}

//DECLARATIONS IN LET
sealed trait DeclarationNode extends EObSecNode
case class LocalDeclarationNode(variable:String,rExpr:EObSecNode) extends DeclarationNode {
  override def children: List[EObSecNode] = List(rExpr)
}
case class TypeAliasDeclarationNode(aliasName: String,objType: AliasDefinableTypeNode) extends DeclarationNode {
  override def children: List[EObSecNode] = List()
}
case class DefTypeNode(name:String,methods:List[MethodDeclarationNode]) extends DeclarationNode {
  override def children: List[EObSecNode] = methods
}


trait AnnotatedFacetedType extends EObSecNode
case class AnnotatedSubtypingFacetedType(left:TypeAnnotation,right: TypeAnnotation) extends AnnotatedFacetedType {
  override def children: List[EObSecNode] = List(left,right)
}
case class AnnotatedExistentialFacetedType(left:TypeAnnotation,inner:List[TypeAnnotation],right: TypeAnnotation) extends AnnotatedFacetedType {
  override def children: List[EObSecNode] = List(left)++ inner ++ List(right)
}
trait TypeAnnotation extends EObSecNode
/**
  * It represent a type in the syntax. It could be Int, String, a reference
  * to a self type variable, a label variable, the top type
  * @param identifier The identifier of the type
  */
case class TypeIdentifier(identifier: String) extends TypeAnnotation {
  override def children: List[EObSecNode] = List()
}
case object LowLabelNode extends TypeAnnotation {
  override def toString: String = "L"
  override def children: List[EObSecNode] = List()
}

case object HighLabelNode extends TypeAnnotation {
  override def toString: String = "H"
  override def children: List[EObSecNode] = List()
}

trait AliasDefinableTypeNode extends TypeAnnotation
sealed trait ObjectTypeAnnotation extends AliasDefinableTypeNode{
  def methods: List[MethodDeclarationNode]
}
case class ObjectTypeNode(selfVar: String,
                          methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  override def children: List[MethodDeclarationNode] = methods
}
case class ExistentialTypeNode(typeVars : AstNodeList[ExistentialVariableDeclarationNode],
                               methods: List[MethodDeclarationNode])
  extends AliasDefinableTypeNode{
  override def children: List[EObSecNode] = List(typeVars) ++ methods
}

case class NoRecursiveObjectTypeNode(methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  override def children: List[MethodDeclarationNode] = methods
}
case class MethodDeclarationNode(name: SimpleIdentifier, mType: MethodTypeNode)  extends  EObSecNode {
  override def children: List[EObSecNode] = List()
}
case class MethodTypeNode(domain: List[AnnotatedFacetedType],
                          codomain: AnnotatedFacetedType)
  extends EObSecNode {
  override def children: List[EObSecNode] = domain ++ List(codomain)
}

case class ExistentialVariableDeclarationNode(name:String,lowerBound:TypeAnnotation) extends EObSecNode{
  override def children: List[EObSecNode] = List(lowerBound)


  var isAster : Boolean = false
  def toAster :this.type = {
    isAster = true
    this
  }
}



