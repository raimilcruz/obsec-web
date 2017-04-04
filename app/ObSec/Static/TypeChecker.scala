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
    case MethodInv(e1, args, m) => {
      val s1 = internalTypeCheck(scope, e1)
      val argTypes =  args.map(param=> internalTypeCheck(scope,param))
      //facet analysis
      if(s1.privateType.containsMethod(m)){
        var mType = s1.privateType.methSig(m)
        if(s1.publicType.containsMethod(m)){
          mType = s1.publicType.methSig(m)
        }

        println(s"Checking subtyping between: ${args} and ${mType.domain}")
        //check the argument count
        if(mType.domain.size != args.size) throw TypeError(s"Method '${m}' : Actual arguments amount must match the formal arguments amount")
        //check subtyping between $mType.domain and s2
        for(pair <- argTypes.zip(mType.domain)){
          if(!subTypingAlgorithm.<::(pair._1,pair._2)) {
            throw TypeError(s"""Invocation of ${m}: Type ${pair._1} (of actual argument) is not subtyping of ${pair._2}""")
          }
        }

        if(s1.publicType.containsMethod(m))mType.codomain
        else SType(mType.codomain.privateType,ObjType.top)

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
      //each method must be well-typed with respect the object type
      for(m <- methods)
      {
        val mType = stype.privateType.methSig(m.name)
        if(mType.domain.size != m.args.size)throw TypeError(s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

        val methodScope = new NestedScope[SType](scope)
        methodScope.add(self, stype)
        m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

        var s = internalTypeCheck(methodScope, m.mBody)
        if(!subTypingAlgorithm.<::(s, mType.codomain))
          throw TypeError(s"Method ${m.name}: the return type in the implementation is not subtype of the return type in the signature")
      }
      stype
    }
    case IfExpr(cond,e1,e2) =>{
      val sCond = internalTypeCheck(scope, cond)
      if(sCond.privateType != BooleanType)
        throw TypeError("Condition of if expression must be boolean")
      val sE1 = internalTypeCheck(scope,e1)
      val sE2 = internalTypeCheck(scope,e2)
      if(sE1 != sE2)
        throw TypeError("Both expression in a if must have the same type")
      //depending on the type of the condiction we should lift the public type of the resulting type
      //TODO: Implement this properly: I should check for the empty object type
      if(sCond.publicType!=BooleanType)
        SType(sE1.privateType,ObjType.top)
      else sE1
    }
  }

  //TODO: Finish the inner recursion
  def removeLabelShortcut(stype: SType): SType = stype match {
    case SType(x,LowLabel) => SType(x,x)
    case SType(x,HighLabel)=> SType(x,ObjType.top)
    case x => x
  }

  //TODO: Finish the inner recursion
  def removeLabelShortcutFromExpr(x: ObSecExpr) = x match {
    case Obj(self,stype,methods)=> Obj(self,removeLabelShortcut(stype),methods)
    case x =>x
  }
}
object TypeChecker{
  def apply(x: ObSecExpr) = {
    val tp = new TypeChecker()
    val expr = tp.removeLabelShortcutFromExpr(x)
    tp.typeCheck(expr)
  }
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
