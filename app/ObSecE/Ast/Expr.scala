

//Here the expression of the extension with existencial.

package ObSecE.Ast

import Common.{AstNode, NoAstNode}

//TODO: Refactor with ObsecGElement
trait EObSecElement{
  var astNode: AstNode = NoAstNode

  def setAstNode(node:AstNode):this.type ={
    if(astNode eq NoAstNode) astNode = node
    this
  }

  var isSynthetic : Boolean = true
  def setIsSynthetic(b:Boolean): this.type ={
    isSynthetic = b
    this
  }
}

case class NodeList[+T <: EObSecElement](elems : List[T]) extends Iterable[T] with EObSecElement {
  override def iterator: Iterator[T] = elems.iterator
}


/**
    * <Expr> ::= <Var> | <Obj> | <MethodInv> | <PrimitiveLiteral>
  *   (| <SurfaceExpr> )
    */

  /**
    * Top Algebraic Data Type for expression in ObSec
    */
  sealed trait EObSecExpr extends EObSecElement{
  }

  /**
    * Represents a variable expression.
    *
    * @param name
    */
  case class Var(name: String) extends EObSecExpr{
    override def toString: String = name
  }

/**
  * Represents an object in ObSec
  *
  * @param selfName       The name of the "self" variable
  * @param selfAscription The ascribed type of the object
  * @param methods        The list of method definitions
  */
case class Obj(selfName: String, selfAscription: STypeE, methods: List[MethodDef]) extends EObSecExpr {

  /**
    * Returns the method implementation for a given method label
    *
    * @param m The method label
    * @return The method definition
    */
  def methodImpl(m: String) = {
    val res = methods.find(x => x.name == m)
    if (res == None)
      throw new Exception("Stuck")
    res.get
  }

  //override def toString: String = s"{$selfName : $selfAscription => ${methods.foldLeft("")((acc,x)=> acc+x)} }"


}

/**
  * Represent an a method invocation expression
  *
  * @param e1 The receiver
  * @param args The actual arguments
  * @param method The method to invoke
  */
case class MethodInv(e1: EObSecExpr,args: NodeList[EObSecExpr], method: String) extends EObSecExpr {
  var methodNameNode :AstNode = NoAstNode
  def setMethodNameNode(methodName:SimpleIdentifier):MethodInv={
    if(methodNameNode eq NoAstNode) methodNameNode = methodName
    this
  }

  def map[T](f: EObSecExpr => EObSecExpr) =
    MethodInv(f(e1), NodeList(args.elems.map(f)), method).setAstNode(astNode)

  //override def toString: String = s"${e1}.$method(${if(args.size==0)"" else args(0) + args.drop(1).foldLeft("")((acc,x)=> acc+","+ x)})"
}

/**
  * Represent a method definition
  *
  * @param name   The name of the method
  * @param args   The list of formal arguments
  * @param mBody  The body expression
  */
case class MethodDef(name: String, args: List[String], mBody: EObSecExpr) extends EObSecElement {
  override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
}

case class IfExpr(cond:EObSecExpr,thenPart:EObSecExpr,elsePart:EObSecExpr) extends EObSecExpr



sealed trait PrimitiveLiteral extends EObSecExpr

//Primitive literals: Int,String, Boolean
case class IntExpr(v: Int) extends PrimitiveLiteral
case class StringExpr(v:String) extends PrimitiveLiteral
case class BooleanExpr(v:Boolean) extends PrimitiveLiteral

/**
  * Surface expressions start here
  */
sealed trait SurfaceExpr extends EObSecExpr

case class ListConstructorExpr(label:LabelE, elems: List[EObSecExpr]) extends EObSecExpr
case class ConsListExpr(elem :EObSecExpr,list:EObSecExpr) extends EObSecExpr

case class LetStarExpr(declarations: List[Declaration],body:EObSecExpr) extends EObSecExpr

sealed trait Declaration extends EObSecElement
case class LocalDeclaration(variable:String,rExpr:EObSecExpr) extends Declaration

//type alias to object type or existential type
case class TypeAlias(aliasName: String,objType: LabelE) extends Declaration
case class TypeDefinition(name:String,methods:List[MethodDeclarationE]) extends Declaration


trait NodeConverts{
  implicit def toNodeList[T <: EObSecElement](l:Iterable[T]):NodeList[T] = new NodeList[T](l.toList)
}
