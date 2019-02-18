package ObSecG.Ast
import Common.AstNode

/**
  * Node:: = Ob
  */

trait ObSecGAstNode extends AstNode{
  def children: List[ObSecGAstNode]
}
sealed trait ObSecGAstExprNode extends ObSecGAstNode

case class SimpleIdentifier(name:String) extends ObSecGAstNode {
  override def children: List[ObSecGAstNode] = List()
}

case class VariableNode(name:String) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List()
}
case class ObjectDefinitionNode(selfName: String,
                                selfAscription: AnnotatedFacetedType,
                                methods: List[MethodDefinitionNode])
  extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = methods
}

case class MethodInvocationNode(e1: ObSecGAstExprNode,
                     types:AstNodeList[TypeAnnotation],
                     args: AstNodeList[ObSecGAstExprNode],
                     method: SimpleIdentifier) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List(e1) ++ args.elems ++ types.elems ++ List(method)
}

case class AstNodeList[+T <: ObSecGAstNode](elems: List[T]) extends ObSecGAstNode {
  if(elems.nonEmpty){
   setPos(elems.head.pos)
    setEndPos(elems.last.endPos)
  }
  override def children: List[ObSecGAstNode] = elems
}

case class MethodDefinitionNode(methodName: SimpleIdentifier, args: AstNodeList[SimpleIdentifier], mBody: ObSecGAstExprNode)
  extends ObSecGAstNode{
  //override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
  override def children: List[ObSecGAstNode] = List(mBody)
  def name:String = methodName.name
}

case class IfExpressionNode(cond:ObSecGAstExprNode,
                  thenPart:ObSecGAstExprNode,
                  elsePart:ObSecGAstExprNode)
  extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List(cond,thenPart,elsePart)
}


case class IntLiteral(v: Int) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List()
}
case class StringLiteral(v:String) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List()
}
case class BooleanLiteral(v:Boolean) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List()
}

case class ListLiteral(label: TypeAnnotation, elems: List[ObSecGAstExprNode]) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = elems
}
case class ConsListOperatorNode(elem :ObSecGAstExprNode,list:ObSecGAstExprNode) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = List(elem,list)
}

case class LetStarExpressionNode(declarations: List[DeclarationNode],body:ObSecGAstExprNode) extends ObSecGAstExprNode {
  override def children: List[ObSecGAstNode] = declarations ++ List(body)
}

//DECLARATIONS IN LET
sealed trait DeclarationNode extends ObSecGAstNode
case class LocalDeclarationNode(variable:String,rExpr:ObSecGAstExprNode) extends DeclarationNode {
  override def children: List[ObSecGAstNode] = List(rExpr)
}
case class TypeAliasDeclarationNode(aliasName: String,objType: ObjectTypeAnnotation) extends DeclarationNode {
  override def children: List[ObSecGAstNode] = List()
}
case class DefTypeNode(name:String,methods:List[MethodDeclarationNode]) extends DeclarationNode {
  override def children: List[ObSecGAstNode] = methods
}



//Type annotations
sealed trait TypeAnnotation extends ObSecGAstNode
case class AnnotatedFacetedType(left:TypeAnnotation,right: TypeAnnotation) extends ObSecGAstNode {
  override def children: List[ObSecGAstNode] = List(left,right)
}

/**
  * It represent a type in the syntax. It could be Int, String, a reference
  * to a self type variable, a label variable, the top type
  * @param identifier The identifier of the type
  */
case class TypeIdentifier(identifier: String) extends TypeAnnotation {
  override def children: List[ObSecGAstNode] = List()
}
case object LowLabelNode extends TypeAnnotation {
  override def toString: String = "L"

  override def children: List[ObSecGAstNode] = List()
}

case object HighLabelNode extends TypeAnnotation {
  override def toString: String = "H"

  override def children: List[ObSecGAstNode] = List()
}
case object ImplicitLabelNode extends  TypeAnnotation {
  override def toString: String = "I*"
  override def children: List[ObSecGAstNode] = List()
}

case class UnionTypeAnnotation(left:TypeAnnotation,right: TypeAnnotation) extends TypeAnnotation {
  override def children: List[ObSecGAstNode] = List(left,right)
}

sealed trait ObjectTypeAnnotation extends TypeAnnotation{
  def methods: List[MethodDeclarationNode]
}
case class ObjectTypeNode(selfVar: String,
                          methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  //override def toString: String = s"OT($selfVar,$methods)"
  override def children: List[ObSecGAstNode] = methods
}
case class NoRecursiveObjectTypeNode(methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  //override def toString: String = s"OT($methods)"
  override def children: List[ObSecGAstNode] = methods
}
case class MethodDeclarationNode(name: SimpleIdentifier, mType: MethodTypeNode)  extends  ObSecGAstNode {
 /* override def toString: String = {
    val typeParams =
      mType
        .typeVars
        .map(tv => s"${tv.toString}").mkString(",")

    var typeParamsPart = if (typeParams.isEmpty) "" else s"[$typeParams]"

    s"{$name$typeParamsPart :" +
      s"${
        mType
          .domain
          .foldLeft("")(
            (acc, x) =>
              acc + " " + x)
      } -> ${mType.codomain}}"
  }*/
  override def children: List[ObSecGAstNode] = List()
}
case class MethodTypeNode(typeVars: List[LabelVariableDeclarationNode],
                          domain: List[AnnotatedFacetedType],
                          codomain: AnnotatedFacetedType)
  extends ObSecGAstNode {
  override def children: List[ObSecGAstNode] = typeVars.map(x=>x:ObSecGAstNode) ++ domain.map(x=>x:ObSecGAstNode) ++ List(codomain)
}

trait LabelVariableDeclarationNode extends ObSecGAstNode{
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
  override def children: List[ObSecGAstNode] = List()
}

/**
  * A label variable with both bounds: m[X:T1..T2]:...
  * @param name The identifier
  * @param lower The lower bound
  * @param upper The upper bound
  */
case class BoundedLabelVariableDeclaration(name:String,lower:TypeAnnotation,upper:TypeAnnotation)
 extends LabelVariableDeclarationNode {
  override def children: List[ObSecGAstNode] = List(lower,upper)
}
case class SubLabelVariableDeclaration(name:String,upper:TypeAnnotation)
  extends LabelVariableDeclarationNode {
  override def children: List[ObSecGAstNode] = List(upper)
}
case class SuperLabelVariableDeclaration(name:String,upper:TypeAnnotation)
  extends LabelVariableDeclarationNode {
  override def children: List[ObSecGAstNode] = List(upper)
}

object NoGObSecNode extends ObSecGAstNode {
  override def children: List[ObSecGAstNode] = List()
}