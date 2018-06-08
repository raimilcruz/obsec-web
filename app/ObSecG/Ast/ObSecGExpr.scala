package ObSecG.Ast

/**
    * <Expr> ::= <Var> | <Obj> | <MethodInv> | <PrimitiveLiteral>
  *   (| <SurfaceExpr> )
    */

  /**
    * Top Algebraic Data Type for expression in ObSec
    */
  sealed trait ObSecGExpr{

  }

  /**
    * Represents a variable expression.
    *
    * @param name
    */
  case class Var(name: String) extends ObSecGExpr{
    override def toString: String = name
  }

  /**
    * Represents an object in ObSec
    *
    * @param selfName       The name of the "self" variable
    * @param selfAscription The ascribed type of the object
    * @param methods        The list of method definitions
    */
  case class Obj(selfName: String, selfAscription: STypeG, methods: List[MethodDef]) extends ObSecGExpr {

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
  case class MethodInv(e1: ObSecGExpr, types:List[LabelG], args: List[ObSecGExpr], method: String) extends ObSecGExpr {
    def map[T](f: ObSecGExpr => ObSecGExpr) =
      MethodInv(f(e1), types, args.map(f), method)

    //override def toString: String = s"${e1}.$method(${if(args.size==0)"" else args(0) + args.drop(1).foldLeft("")((acc,x)=> acc+","+ x)})"
  }

  /**
    * Represent a method definition
    *
    * @param name   The name of the method
    * @param args   The list of formal arguments
    * @param mBody  The body expression
    */
  case class MethodDef(name: String, args: List[String], mBody: ObSecGExpr){
    override def toString: String = s"{$name : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
  }

  case class IfExpr(cond:ObSecGExpr,thenPart:ObSecGExpr,elsePart:ObSecGExpr) extends ObSecGExpr



sealed trait PrimitiveLiteral extends ObSecGExpr

//Primitive literals: Int,String, Boolean
case class IntExpr(v: Int) extends PrimitiveLiteral
case class StringExpr(v:String) extends PrimitiveLiteral
case class BooleanExpr(v:Boolean) extends PrimitiveLiteral

/**
  * Surface expressions start here
  */
sealed trait SurfaceExpr extends ObSecGExpr

case class ListConstructorExpr(elems: List[ObSecGExpr]) extends ObSecGExpr
case class ConsListExpr(elem :ObSecGExpr,list:ObSecGExpr) extends ObSecGExpr

case class LetStarExpr(declarations: List[Declaration],body:ObSecGExpr) extends ObSecGExpr

sealed trait Declaration
case class LocalDeclaration(variable:String,rExpr:ObSecGExpr) extends Declaration
case class TypeAlias(aliasName: String,objType: ObjectType) extends Declaration
case class TypeDefinition(name:String,methods:List[MethodDeclarationG]) extends Declaration





