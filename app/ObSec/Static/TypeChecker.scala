package ObSec.Static

import ObSec.Ast._

import scala.collection.mutable


/**
  * Created by rcc on 3/30/2017.
  */
class TypeChecker () {

  val subTypingAlgorithm = new AmadioCardelliSubtyping()
  val wfChecker = new WellFormedChecker(new ErrorCollector)

  def typeCheck(x: ObSecExpr):SType = internalTypeCheck(new Scope, x)

  def typeCheck(scope: Scope[SType], expr: ObSecExpr): SType = internalTypeCheck(scope,expr)




  def internalTypeCheck(scope: Scope[SType], expr: ObSecExpr): SType = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => SType(IntType,IntType)
    case BooleanExpr(_) => SType(BooleanType,BooleanType)
    case StringExpr(_) => SType(StringType,StringType)
    case MethodInv(e1, args, m) => {
      val s1 = internalTypeCheck(scope, e1)
      val argTypes =  args.map(param=> internalTypeCheck(scope,param))
      println(s"Args: ${argTypes}")
      //facet analysis
      if(s1.privateType.containsMethod(m)){
        var mType = s1.privateType.methSig(m)
        println(s"Method type : ${mType}")
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
      if(!wfChecker.isWellFormed(stype))
        throw TypeError(s"Security type is not well-formed : ${wfChecker.errorCollector.errors}")
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
          throw TypeError(s"Definition of method '${m.name}': the return type in the implementation (${s}) is not subtype of the return type in the signature (${mType.codomain})")
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
    case LetStarExpr(declarations,body) =>
      val letScope = new NestedScope[SType](scope)

      //verify well-formedness of the type aliases
      var typeAliases = declarations.filter(d=>d.isInstanceOf[TypeAlias]).map(ld=>ld.asInstanceOf[TypeAlias])

      val varDeclarations = declarations.filter(d=>d.isInstanceOf[LocalDeclaration]).map(ld=>ld.asInstanceOf[LocalDeclaration])
      varDeclarations.foreach(d => letScope.add(d.variable,internalTypeCheck(letScope,d.rExpr)))
      internalTypeCheck(letScope,body)
    case ListConstructorExpr(elems) =>
      if(!elems.forall(e=> subTypingAlgorithm.<::(internalTypeCheck(scope,e),SType(StringType,StringType))))
        throw TypeError("Elements of a list must be of type String<String")
      SType(StringListType,StringListType)
  }

}
object TypeChecker{
  def apply(x: ObSecExpr) = {
    val tp = new TypeChecker()
    tp.typeCheck(x)
  }
}





class Scope[T] {

  def lookup(x: String): T  = throw new Error(s"Variable ${x} not in Scope")

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
