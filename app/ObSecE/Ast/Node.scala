package ObSecE.Ast

import Common.{AstNode, TypeAnnotation}

case class SimpleIdentifier(name:String) extends AstNode {
  override def children: List[AstNode] = List()
}

case class VariableNode(name:String) extends AstNode {
  override def children: List[AstNode] = List()
}
case class ObjectDefinitionNode(selfName: String,
                                selfAscription: AnnotatedFacetedType,
                                methods: List[MethodDefinitionNode])
  extends AstNode {
  override def children: List[MethodDefinitionNode] = methods
}
case class MethodInvocationNode(e1: AstNode,
                                types:AstNodeList[TypeAnnotation],
                                args: AstNodeList[AstNode],
                                method: SimpleIdentifier) extends AstNode {
  override def children: List[AstNode] = List(e1) ++ args.elems ++ types.elems ++ List(method)
}
case class AstNodeList[+T <: AstNode](elems: List[T]) extends AstNode {
  if(elems.nonEmpty){
    setPos(elems.head.pos)
    setEndPos(elems.last.endPos)
  }
  override def children: List[AstNode] = elems
}

case class MethodDefinitionNode(methodName: SimpleIdentifier, args: AstNodeList[SimpleIdentifier], mBody: AstNode )
  extends AstNode {
  //override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
  override def children: List[AstNode] = List(mBody)
  def name:String = methodName.name
}

case class IfExpressionNode(cond:AstNode ,
                            thenPart:AstNode ,
                            elsePart:AstNode )
  extends AstNode  {
  override def children: List[AstNode ] = List(cond,thenPart,elsePart)
}


case class IntLiteral(v: Int) extends AstNode  {
  override def children: List[AstNode ] = List()
}
case class StringLiteral(v:String) extends AstNode  {
  override def children: List[AstNode ] = List()
}
case class BooleanLiteral(v:Boolean) extends AstNode  {
  override def children: List[AstNode ] = List()
}

case class ListLiteral(label: TypeAnnotation, elems: List[AstNode ]) extends AstNode  {
  override def children: List[AstNode ] = elems
}
case class ConsListOperatorNode(elem :AstNode ,list:AstNode) extends AstNode  {
  override def children: List[AstNode ] = List(elem,list)
}

case class LetStarExpressionNode(declarations: List[DeclarationNode],body:AstNode ) extends AstNode  {
  override def children: List[AstNode ] = declarations ++ List(body)
}

//DECLARATIONS IN LET
sealed trait DeclarationNode extends AstNode
case class LocalDeclarationNode(variable:String,rExpr:AstNode ) extends DeclarationNode {
  override def children: List[AstNode ] = List(rExpr)
}
case class TypeAliasDeclarationNode(aliasName: String,objType: ObjectTypeAnnotation) extends DeclarationNode {
  override def children: List[AstNode ] = List()
}
case class DefTypeNode(name:String,methods:List[MethodDeclarationNode]) extends DeclarationNode {
  override def children: List[AstNode ] = methods
}


case class AnnotatedFacetedType(left:TypeAnnotation,right: TypeAnnotation) extends AstNode {
  override def children: List[AstNode] = List(left,right)
}

/**
  * It represent a type in the syntax. It could be Int, String, a reference
  * to a self type variable, a label variable, the top type
  * @param identifier The identifier of the type
  */
case class TypeIdentifier(identifier: String) extends TypeAnnotation {
  override def children: List[AstNode] = List()
}
case object LowLabelNode extends TypeAnnotation {
  override def toString: String = "L"
  override def children: List[AstNode] = List()
}

case object HighLabelNode extends TypeAnnotation {
  override def toString: String = "H"
  override def children: List[AstNode] = List()
}

sealed trait ObjectTypeAnnotation extends TypeAnnotation{
  def methods: List[MethodDeclarationNode]
}
case class ObjectTypeNode(selfVar: String,
                          methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  override def children: List[MethodDeclarationNode] = methods
}
case class NoRecursiveObjectTypeNode(methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  override def children: List[MethodDeclarationNode] = methods
}
case class MethodDeclarationNode(name: SimpleIdentifier, mType: MethodTypeNode)  extends  AstNode {
  override def children: List[AstNode] = List()
}
case class MethodTypeNode(typeVars: List[LabelVariableDeclarationNode],
                          domain: List[AnnotatedFacetedType],
                          codomain: AnnotatedFacetedType)
  extends AstNode {
  override def children: List[AstNode] = typeVars.map(x=>x:AstNode) ++ domain.map(x=>x:AstNode) ++ List(codomain)
}

trait LabelVariableDeclarationNode extends AstNode{
  def name:String

  var isAster : Boolean = false
  def toAster :this.type = {
    isAster = true
    this
  }
}

/**
  * A simple label variable m[X]: ...
  * @param name the idenfitifer of the variable
  */
case class SimpleLabelVariableDeclarationNode(name:String)
  extends LabelVariableDeclarationNode {
  override def children: List[AstNode] = List()
}

