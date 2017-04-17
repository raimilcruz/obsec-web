package ObSec.Ast

  /**
    * <Expr> ::= <Var> | <Obj> | <MethodInv>
    */

  /**
    * Top Algebraic Data Type for expression in ObSec
    */
  sealed trait ObSecExpr{

  }

  /**
    * Represents a variable expression.
    *
    * @param name
    */
  case class Var(name: String) extends ObSecExpr{
    override def toString: String = name


  }

  /**
    * Represents an object in ObSec
    *
    * @param selfName       The name of the "self" variable
    * @param selfAscription The ascribed type of the object
    * @param methods        The list of method definitions
    */
  case class Obj(selfName: String, selfAscription: SType, methods: List[MethodDef]) extends ObSecExpr {

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

    override def toString: String = s"{$selfName : $selfAscription => ${methods.foldLeft("")((acc,x)=> acc+x)} }"


  }

  /**
    * Represent an a method invocation expression
    *
    * @param e1 The receiver
    * @param args The actual arguments
    * @param method The method to invoke
    */
  case class MethodInv(e1: ObSecExpr, args: List[ObSecExpr], method: String) extends ObSecExpr {
    def map[T](f: ObSecExpr => ObSecExpr) = new MethodInv(f(e1), args.map(f), method)

    //override def toString: String = s"${e1}.$method(${if(args.size==0)"" else args(0) + args.drop(1).foldLeft("")((acc,x)=> acc+","+ x)})"
  }

  /**
    * Represent a method definition
    *
    * @param name   The name of the method
    * @param args   The list of formal arguments
    * @param mBody  The body expression
    */
  case class MethodDef(name: String, args: List[String], mBody: ObSecExpr){
    override def toString: String = s"{${name} : ${args.foldLeft("")((acc,x)=> acc + " " +x)} = $mBody}"
  }

  case class IfExpr(cond:ObSecExpr,thenPart:ObSecExpr,elsePart:ObSecExpr) extends ObSecExpr


/**
  * Surface expressions start here
  */
sealed trait SurfaceExpr extends ObSecExpr
sealed trait SurfaceValExpr extends ObSecExpr

case class IntExpr(v: Int) extends SurfaceValExpr
case class StringExpr(v:String) extends SurfaceValExpr
case class BooleanExpr(v:Boolean) extends SurfaceValExpr
case class ListConstructorExpr(elems: List[ObSecExpr]) extends ObSecExpr

case class LetStarExpr(declarations: List[Declaration],body:ObSecExpr) extends ObSecExpr

sealed trait Declaration
case class LocalDeclaration(variable:String,rExpr:ObSecExpr) extends Declaration
case class TypeAlias(aliasName: String,objType: ObjType) extends Declaration
case class TypeDefinition(name:String,methods:List[MethodDeclaration]) extends Declaration




