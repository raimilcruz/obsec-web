package ObSec.Ast

  /**
    * <Expr> ::= <Var> | <Obj> | <MethodInv>
    */

  /**
    * Top Algebraic Data Type for expression in ObSec
    */
  sealed trait ObSecExpr

  /**
    * Represents a variable expression.
    *
    * @param name
    */
  case class Var(name: String) extends ObSecExpr

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
  }

  /**
    * Represent an a method invocation expression
    *
    * @param e1
    * @param e2
    * @param method
    */
  case class MethodInv(e1: ObSecExpr, e2: ObSecExpr, method: String) extends ObSecExpr {

    def map[T](f: ObSecExpr => ObSecExpr) =
      new MethodInv(f(e1), f(e2), method)
  }

  /**
    * Represent a method definition
    *
    * @param name    The name of the method
    * @param argName The argument name
    * @param mBody   The body expression
    */
  case class MethodDef(name: String, argName: String, mBody: ObSecExpr) {
    def mapOnBody(f: ObSecExpr => ObSecExpr) = new MethodDef(name, argName, f(mBody))
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


/**
  * Primitive operations
  */
case class PrimOp(op:String,operands: ObSecExpr*) extends SurfaceExpr







