package ObSec.Static

import ObSec.Ast._
import Common._


/**
  * Created by rcc on 3/30/2017.
  */
class TypeChecker() {

  val subTypingAlgorithm = new AmadioCardelliSubtyping()
  val wfChecker = new WellFormedChecker(new ErrorCollector)

  def typeCheck(x: ObSecExpr): SType = internalTypeCheck(new Scope, new Scope, x)

  def typeCheck(scope: Scope[SType], expr: ObSecExpr): SType = internalTypeCheck(scope, new Scope, expr)


  def internalTypeCheck(scope: Scope[SType], aliasScope: Scope[Type], expr: ObSecExpr): SType = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => SType(IntType, IntType)
    case BooleanExpr(_) => SType(BooleanType, BooleanType)
    case StringExpr(_) => SType(StringType, StringType)
    case MethodInv(e1, args, m) => {
      val s1 = internalTypeCheck(scope, aliasScope, e1)
      val argTypes = args.map(param => internalTypeCheck(scope, aliasScope, param))
      //println(s"Args: ${argTypes}")
      //facet analysis
      if (s1.privateType.containsMethod(m)) {
        var mType = s1.privateType.methSig(m)
        //println(s"Method type : ${mType}")
        if (s1.publicType.containsMethod(m)) {
          mType = s1.publicType.methSig(m)
        }

        //println(s"Checking subtyping between: ${args} and ${mType.domain}")
        //check the argument count
        if (mType.domain.size != args.size) throw CommonError.genericError(expr,s"Method '${m}' : Actual arguments amount must match the formal arguments amount")
        //check subtyping between $mType.domain and s2
        for (pair <- argTypes.zip(mType.domain)) {
          if (!subTypingAlgorithm.<::(pair._1, pair._2)) {
            throw CommonError.genericError(expr,s"""Invocation of ${m}: Type ${pair._1} (of actual argument) is not subtyping of ${pair._2}""")
          }
        }

        if (s1.publicType.containsMethod(m)) mType.codomain
        else SType(mType.codomain.privateType, ObjType.top)

      }
      else
        throw CommonError.genericError(expr,s"Method ${m} not in ${s1.privateType}")
    }
    case Obj(self, selfType, methods) =>
      val stype = closeAliases(aliasScope, selfType)
      //verify well-formed for stype
      if (!wfChecker.isWellFormed(stype))
        throw CommonError.genericError(expr,s"Security type: '${stype}' is not well-formed : ${wfChecker.errorCollector.errors}")
      //the private type must be an object type
      if (!stype.privateType.isInstanceOf[ObjType])
        throw CommonError.genericError(expr,"The private facet must be an object type")
      //an object can not have repeated method definitions
      if (methods.map(x => x.name).distinct.size != methods.size)
        throw CommonError.genericError(expr,"An object can not have repeated method names")
      //both methods list: in type and in definition must have the same elements
      val privateMethodNames = stype.privateType.asInstanceOf[ObjType].methods.map(x => x.name)
      val methodDefNames= methods.map(x => x.name)
      val methsNoDef = privateMethodNames.filter(x=> !methodDefNames.contains(x))
      if (methsNoDef.nonEmpty) {
        throw CommonError.genericError(expr,s"There must exist a method definition of each method signature. Missing method definition for: ${methodDefNames.foldLeft("")((acc,m)=>acc+" " +m)}.")
      }
      val methsNoSignature = methodDefNames.filter(x => !privateMethodNames.contains(x))
      if (methsNoSignature.nonEmpty) {
        throw CommonError.genericError(expr,s"There must exist a method signature for each method definition. Missing method signature for: ${methsNoSignature.foldLeft("")((acc,m)=>acc+" " +m)}.")
      }
      //each method must be well-typed with respect the object type
      for (m <- methods) {
        val mType = stype.privateType.methSig(m.name)
        if (mType.domain.size != m.args.size) CommonError.genericError(expr,s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

        val methodScope = new NestedScope[SType](scope)
        methodScope.add(self, stype)
        m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

        var s = internalTypeCheck(methodScope, aliasScope, m.mBody)
        if (!subTypingAlgorithm.<::(s, mType.codomain))
          throw CommonError.genericError(expr,s"Definition of method '${m.name}': the return type in the implementation (${s}) is not subtype of the return type in the signature (${mType.codomain})")
      }
      stype
    case IfExpr(cond, e1, e2) =>
      val sCond = internalTypeCheck(scope, aliasScope, cond)
      if (sCond.privateType != BooleanType)
        throw CommonError.genericError(expr,"Condition of if expression must be boolean")
      val sE1 = internalTypeCheck(scope, aliasScope, e1)
      val sE2 = internalTypeCheck(scope, aliasScope, e2)
      if (sE1 != sE2)
        throw CommonError.genericError(expr,"Both branches of an if expression must have the same type")
      //depending on the type of the condiction we should lift the public type of the resulting type
      //TODO: Implement this properly: I should check for the empty object type
      if (sCond.publicType != BooleanType)
        SType(sE1.privateType, ObjType.top)
      else sE1
    case LetStarExpr(declarations, body) =>
      val newTypeAliasScope = new NestedScope[Type](aliasScope)
      val letScope = new NestedScope[SType](scope)

      val typeDefs = declarations.filter(d => d.isInstanceOf[TypeDefinition] || d.isInstanceOf[TypeAlias])
      for (td <- typeDefs) {
        td match{
          case deftype:TypeDefinition =>
            val closedType = closeAliases(newTypeAliasScope.toList, ObjType(TypeVar(deftype.name), deftype.methods))
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw CommonError.genericError(expr,s"Type declaration '${deftype.name}' declaration is not well-formed: ${closedType}")
            newTypeAliasScope.add(deftype.name, closedType)
          case ta:TypeAlias =>
            val closedType = closeAliases(newTypeAliasScope.toList, ta.objType)
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw CommonError.genericError(expr,s"Type in the type alias '${ta.aliasName}' declaration is not well-formed: ${closedType}")
            newTypeAliasScope.add(ta.aliasName, closedType)
        }
      }

      val varDeclarations = declarations.filter(d => d.isInstanceOf[LocalDeclaration]).map(ld => ld.asInstanceOf[LocalDeclaration])
      varDeclarations.foreach(d => letScope.add(d.variable, internalTypeCheck(letScope, newTypeAliasScope, d.rExpr)))
      internalTypeCheck(letScope, newTypeAliasScope, body)
    case ListConstructorExpr(elems) =>
      if (!elems.forall(e => subTypingAlgorithm.<::(internalTypeCheck(scope, aliasScope, e), SType(StringType, StringType))))
        throw CommonError.genericError(expr,"Arguments of 'mklist' must be of type String<String")
      SType(StringGListType(StringType), StringGListType(StringType))
    case ConsListExpr(elem,list) =>
      val tList = internalTypeCheck(scope, aliasScope, list)
      val tElem = internalTypeCheck(scope, aliasScope, elem)
      throw new NotImplementedError()
  }


  def closeAliases(aliasScope: Scope[Type], sType: SType): SType = {
    //TODO: Check problem with alias with same name at different scopes
    val aliases = aliasScope.toList
    SType(closeAliases(aliases, sType.privateType), closeAliases(aliases, sType.publicType))
  }

  def closeAliases(aliases: List[(String, Type)], theType: Type): Type = aliases match {
    case List() => theType
    case binding :: tail => closeAliases(tail, TypeSubst.subst(theType, binding._1, binding._2))
  }

}

object TypeChecker {
  def apply(x: ObSecExpr) = {
    val tp = new TypeChecker()
    tp.typeCheck(x)
}
}

