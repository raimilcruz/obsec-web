package ObSecG.Static

import ObSecG.Ast._
import Common._

/**
  * Created by racruz on 24-08-2017.
  */
class TypeCheckerG {
  val subTypingAlgorithm = new AmadioCardelliSubtypingG()
  val wfChecker = new WellFormedCheckerG(new ErrorCollector)
  def typeCheck(x: ObSecGExpr): STypeG = internalTypeCheck(new Scope, new Scope,x)

  def internalTypeCheck(scope: Scope[STypeG], aliasScope: Scope[TypeG], expr: ObSecGExpr): STypeG = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => STypeG(IntType, IntType)
    case BooleanExpr(_) => STypeG(BooleanType, BooleanType)
    case StringExpr(_) => STypeG(StringType, StringType)
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
        if (mType.domain.size != args.size) throw TypeError(s"Method '${m}' : Actual arguments amount must match the formal arguments amount")
        //check subtyping between $mType.domain and s2
        for (pair <- argTypes.zip(mType.domain)) {
          if (!subTypingAlgorithm.<::(pair._1, pair._2)) {
            throw TypeError(s"""Invocation of ${m}: Type ${pair._1} (of actual argument) is not subtyping of ${pair._2}""")
          }
        }

        if (s1.publicType.containsMethod(m)) mType.codomain
        else STypeG(mType.codomain.privateType, ObjectType.top)

      }
      else
        throw TypeError(s"Method ${m} not in ${s1.privateType}")
    }
    case Obj(self, selfType, methods) =>
      val stype = closeAliases(aliasScope, selfType)
      //verify well-formed for stype
      if (!wfChecker.isWellFormed(stype))
        throw TypeError(s"Security type: '${stype}' is not well-formed : ${wfChecker.errorCollector.errors}")
      //the private type must be an object type
      if (!stype.privateType.isInstanceOf[ObjectType])
        throw TypeError("The private facet must be an object type")
      //an object can not have repeated method definitions
      if (methods.map(x => x.name).distinct.size != methods.size)
        throw TypeError("An object can not have repeated method names")
      //both methods list: in type and in definition must have the same elements
      val privateMethodNames = stype.privateType.asInstanceOf[ObjectType].methods.map(x => x.name)
      val methodDefNames= methods.map(x => x.name)
      val methsNoDef = privateMethodNames.filter(x=> !methodDefNames.contains(x))
      if (methsNoDef.nonEmpty) {
        throw TypeError(s"There must exist a method definition of each method signature. Missing method definition for: ${methodDefNames.foldLeft("")((acc,m)=>acc+" " +m)}.")
      }
      val methsNoSignature = methodDefNames.filter(x => !privateMethodNames.contains(x))
      if (methsNoSignature.nonEmpty) {
        throw TypeError(s"There must exist a method signature for each method definition. Missing method signature for: ${methsNoSignature.foldLeft("")((acc,m)=>acc+" " +m)}.")
      }
      //each method must be well-typed with respect the object type
      for (m <- methods) {
        val mType = stype.privateType.methSig(m.name)
        if (mType.domain.size != m.args.size) throw TypeError(s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

        val methodScope = new NestedScope[STypeG](scope)
        methodScope.add(self, stype)
        m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

        var s = internalTypeCheck(methodScope, aliasScope, m.mBody)
        if (!subTypingAlgorithm.<::(s, mType.codomain))
          throw TypeError(s"Definition of method '${m.name}': the return type in the implementation (${s}) is not subtype of the return type in the signature (${mType.codomain})")
      }
      stype
    case IfExpr(cond, e1, e2) =>
      val sCond = internalTypeCheck(scope, aliasScope, cond)
      if (sCond.privateType != BooleanType)
        throw TypeError("Condition of if expression must be boolean")
      val sE1 = internalTypeCheck(scope, aliasScope, e1)
      val sE2 = internalTypeCheck(scope, aliasScope, e2)
      if (sE1 != sE2)
        throw TypeError("Both branches of an if expression must have the same type")
      //depending on the type of the condiction we should lift the public type of the resulting type
      //TODO: Implement this properly: I should check for the empty object type
      if (sCond.publicType != BooleanType)
        STypeG(sE1.privateType, ObjectType.top)
      else sE1
    case LetStarExpr(declarations, body) =>
      val newTypeAliasScope = new NestedScope[TypeG](aliasScope)
      val letScope = new NestedScope[STypeG](scope)

      val typeDefs = declarations.filter(d => d.isInstanceOf[TypeDefinition] || d.isInstanceOf[TypeAlias])
      for (td <- typeDefs) {
        td match{
          case deftype:TypeDefinition =>
            val closedType = closeAliases(newTypeAliasScope.toList(), ObjectType(deftype.name, deftype.methods))
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw TypeError(s"Type declaration '${deftype.name}' declaration is not well-formed: ${closedType}")
            newTypeAliasScope.add(deftype.name, closedType)
          case ta:TypeAlias =>
            val closedType = closeAliases(newTypeAliasScope.toList(), ta.objType)
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw TypeError(s"Type in the type alias '${ta.aliasName}' declaration is not well-formed: ${closedType}")
            newTypeAliasScope.add(ta.aliasName, closedType)
        }
      }

      val varDeclarations = declarations.filter(d => d.isInstanceOf[LocalDeclaration]).map(ld => ld.asInstanceOf[LocalDeclaration])
      varDeclarations.foreach(d => letScope.add(d.variable, internalTypeCheck(letScope, newTypeAliasScope, d.rExpr)))
      internalTypeCheck(letScope, newTypeAliasScope, body)
    case ListConstructorExpr(elems) =>
      if (!elems.forall(e => subTypingAlgorithm.<::(internalTypeCheck(scope, aliasScope, e), STypeG(StringType, StringType))))
        throw TypeError("Arguments of 'mklist' must be of type String<String")
      STypeG(StringGListType(StringType), StringGListType(StringType))
    case ConsListExpr(elem,list) =>
      val tList = internalTypeCheck(scope, aliasScope, list)
      val tElem = internalTypeCheck(scope, aliasScope, elem)
      throw new NotImplementedError()
  }

  def closeAliases(aliasScope: Scope[TypeG], sType: STypeG): STypeG = {
    //TODO: Check problem with alias with same name at different scopes
    val aliases = aliasScope.toList()
    STypeG(closeAliases(aliases, sType.privateType), closeAliases(aliases, sType.publicType))
  }

  def closeAliases(aliases: List[(String, TypeG)], theType: TypeG): TypeG = aliases match {
    case List() => theType
    case binding :: tail => closeAliases(tail, TypeSubstG.subst(theType, binding._1, binding._2))
  }


}
object TypeCheckerG{
  def apply(x:Any):TypeG = throw new NotImplementedError
}
