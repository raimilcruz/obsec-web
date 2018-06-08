package ObSecG.Ast

/**
  * Node:: = Ob
  */

trait ObSecGAstNode
sealed trait ObSecGAstExprNode extends ObSecGAstNode

case class VariableNode(name:String) extends ObSecGAstExprNode
case class ObjectDefinitionNode(selfName: String,
                                selfAscription: AnnotatedFacetedType,
                                methods: List[MethodDefinitionNode])
  extends ObSecGAstExprNode

case class MethodInvocationNode(e1: ObSecGAstExprNode,
                     types:List[TypeAnnotation],
                     args: List[ObSecGAstExprNode],
                     method: String) extends ObSecGAstExprNode {
}


case class MethodDefinitionNode(name: String, args: List[String], mBody: ObSecGAstExprNode)
  extends ObSecGAstNode{
  //override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
}

case class IfExpressionNode(cond:ObSecGAstExprNode,
                  thenPart:ObSecGAstExprNode,
                  elsePart:ObSecGAstExprNode)
  extends ObSecGAstExprNode


case class IntLiteral(v: Int) extends ObSecGAstExprNode
case class StringLiteral(v:String) extends ObSecGAstExprNode
case class BooleanLiteral(v:Boolean) extends ObSecGAstExprNode

case class ListLiteral(elems: List[ObSecGAstExprNode]) extends ObSecGAstExprNode
case class ConsListOperatorNode(elem :ObSecGAstExprNode,list:ObSecGAstExprNode) extends ObSecGAstExprNode

case class LetStarExpressionNode(declarations: List[DeclarationNode],body:ObSecGAstExprNode) extends ObSecGAstExprNode

//DECLARATIONS IN LET
sealed trait DeclarationNode extends ObSecGAstNode
case class LocalDeclarationNode(variable:String,rExpr:ObSecGAstExprNode) extends DeclarationNode
case class TypeAliasDeclarationNode(aliasName: String,objType: ObjectTypeAnnotation) extends DeclarationNode
case class DefTypeNode(name:String,methods:List[MethodDeclarationNode]) extends DeclarationNode



//Type annotations
sealed trait TypeAnnotation
case class AnnotatedFacetedType(left:TypeAnnotation,right: TypeAnnotation)

/**
  * It represent a type in the syntax. It could be Int, String, a reference
  * to a self type variable, a label variable, the top type
  * @param identifier The identifier of the type
  */
case class TypeIdentifier(identifier: String) extends TypeAnnotation
sealed trait ObjectTypeAnnotation extends TypeAnnotation{
  def methods: List[MethodDeclarationNode]
}
case class ObjectTypeNode(selfVar: String,
                          methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  //override def toString: String = s"OT($selfVar,$methods)"
}
case class NoRecursiveObjectTypeNode(methods: List[MethodDeclarationNode])
  extends ObjectTypeAnnotation {
  //override def toString: String = s"OT($methods)"
}
case class MethodDeclarationNode(name: String, mType: MethodTypeNode)  extends  ObSecGAstNode {
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
}
case class MethodTypeNode(typeVars: List[LabelVariableDeclarationNode],
                          domain: List[AnnotatedFacetedType],
                          codomain: AnnotatedFacetedType)
  extends ObSecGAstNode {
}

trait LabelVariableDeclarationNode extends ObSecGAstNode{
  def name:String
}

/**
  * A simple label variable m[X]: ...
  * @param name the idenfitifer of the variable
  */
case class SimpleLabelVariableDeclarationNode(name:String)
  extends LabelVariableDeclarationNode

/**
  * A label variable with both bounds: m[X:T1..T2]:...
  * @param name The identifier
  * @param lower The lower bound
  * @param upper The upper bound
  */
case class BoundedLabelVariableDeclaration(name:String,lower:TypeAnnotation,upper:TypeAnnotation)
 extends LabelVariableDeclarationNode
case class SubLabelVariableDeclaration(name:String,upper:TypeAnnotation)
  extends LabelVariableDeclarationNode
case class SuperLabelVariableDeclaration(name:String,upper:TypeAnnotation)
  extends LabelVariableDeclarationNode
