package ObSec.Static

import ObSec.Ast._

import scala.collection.mutable


/**
  * Created by rcc on 3/30/2017.
  */
class TypeChecker () {
  val subTypingAlgorithm = new AmadioCardelliSubtyping()
  def typeCheck(x: ObSecExpr) = internalTypeCheck(new Scope, x)

  private def internalTypeCheck(scope: Scope[SType], expr: ObSecExpr): SType = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => SType(IntType,IntType)
    case BooleanExpr(_) => SType(BooleanType,BooleanType)
    case StringExpr(_) => SType(StringType,StringType)
    case MethodInv(e1, e2, m) => {
      val s1 = internalTypeCheck(scope, e1)
      val s2 = internalTypeCheck(scope, e2)
      //facet analysis
      if(s1.privateType.containsMethod(m)){
        var mType = s1.privateType.methSig(m)
        if(s1.publicType.containsMethod(m)){
          mType = s1.privateType.methSig(m)
        }
        //check subtyping between $mType.domain and s2
        if(subTypingAlgorithm.<::(s2,mType.domain))mType.codomain
        else throw TypeError(s"Type ${s2} is not subtyping of ${mType.domain} in ${expr}")
      }
      else
        throw TypeError(s"Method ${m} not in ${s1.privateType}")
    }
    case Obj(self,stype,methods)=>{
      //verify well-formed for stype
      if(!WellFormedChecker.isWellFormed(stype))
        throw TypeError(s"Object type is not well-formed in ${expr}")
      //the private type must be an object type
      if(!stype.privateType.isInstanceOf[ObjType])
        throw TypeError("The private facet must be an object type")
      //an object can not have repeated method definitions
      if(methods.map(x=>x.name).distinct.size !=methods.size)
        throw TypeError("Object can not have repeated method names")
      //both methods list: in type and in definition must have the same elements
      if(stype.privateType.asInstanceOf[ObjType].methods.map(x=>x.name).toSet != methods.map(x=>x.name).toSet)
        throw TypeError("The private facet must have a method type for each method definition")
      //each method must be well-typed with respect the objec type
      for(m <- methods)
      {
        val mType = stype.privateType.methSig(m.name)
        val methodScope = new NestedScope[SType](scope)
        methodScope.add(self, stype)
        methodScope.add(m.argName, mType.domain)

        var s = internalTypeCheck(methodScope, m.mBody)
        if(!subTypingAlgorithm.<::(s, mType.codomain))
          throw TypeError(s"Method ${m.name} in object $expr")
      }
      stype
    }
    case IfExpr(cond,e1,e2) =>{
      val sCond = internalTypeCheck(scope, cond)
      if(sCond.privateType != BooleanType)
        throw TypeError("Condition of if expression must be boolean")
      val sE1 = internalTypeCheck(scope,e1);
      val sE2 = internalTypeCheck(scope,e2);
      if(sE1 != sE2)
        throw TypeError("Both expression in a if must have the same type")
      //depending on the type of the condiction we should lift the public type of the resulting type
      //TODO: Implement this properly: I should check for the empty object type
      if(sCond.publicType!=BooleanType)
        SType(sE1.privateType,ObjType.top)
      else sE1
    }
  }
}
object TypeChecker{
  def apply(x: ObSecExpr) = new TypeChecker().typeCheck(x)
}





class Scope[T] {
   def lookup(x: String): T  = throw new Error("Not variable in Scope")

  def contains(x: String) = false

  def add(x:String, v: T): Unit = throw new Error("Not a functional scope")

}

case class TypeError(str:String) extends Error


class NestedScope[T](parent: Scope[T]) extends Scope[T] {
  protected val bindings: mutable.HashMap[String, T] = mutable.HashMap.empty[String, T]
  override def lookup(x: String): T = {
    if (bindings.contains(x)) bindings(x)
    else parent.lookup(x)
  }

  override def contains(x: String): Boolean = bindings.contains(x) || parent.contains(x)

  override def add(x: String, v: T): Unit = {
    if(bindings.contains(x))
      throw new Error("Variable is already in this scope")
    bindings(x)= v
  }
}
