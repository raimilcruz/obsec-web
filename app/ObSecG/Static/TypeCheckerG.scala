package ObSecG.Static

import ObSecG.Ast._
import Common._


trait ITypeChecker {
  def typeCheck(x: ObSecGExpr): STypeG

  def typeCheck(genVarEnv: Environment[TypeG], scope: Scope[STypeG],
                aliasScope: Scope[TypeG], expr: ObSecGExpr): STypeG
}

/**
  * Created by racruz on 24-08-2017.
  */
class TypeCheckerG extends ITypeChecker {
  val auxiliaryFunctions = new AuxiliaryFunctions
  val subTypingAlgorithm = new AmadioCardelliSubtypingG()
  val wfChecker = new WellFormedCheckerG(new ErrorCollector)


  def typeCheck(x: ObSecGExpr): STypeG =
    typeCheck(new EmptyEnvironment[TypeG], new Scope, new Scope, x)



  /**
    *
    * @param genVarEnv  The generic variable subtyping constraint environment(eg::= X<:T,..)
    * @param scope      The value environment
    * @param aliasScope The environment for type aliases
    * @param expr       The expression
    * @return
    */
  def typeCheck(genVarEnv: Environment[TypeG],
                scope: Scope[STypeG],
                aliasScope: Scope[TypeG],
                expr: ObSecGExpr): STypeG = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => STypeG(IntType, IntType)
    case BooleanExpr(_) => STypeG(BooleanType, BooleanType)
    case StringExpr(_) => STypeG(StringType, StringType)
    case MethodInv(e1, types, args, m) => {
      val s1 = typeCheck(genVarEnv, scope, aliasScope, e1)
      val argTypes = args.map(param =>
        typeCheck(genVarEnv, scope, aliasScope, param))
      //println(s"Args: ${argTypes}")
      //facet analysis
      //get the upper bound of both types
      val privateFacet = auxiliaryFunctions.tBound(genVarEnv, s1.privateType)
      println("typeCheck:" + s"private facet:$privateFacet")
      if (privateFacet.containsMethod(m)) {
        var mType = privateFacet.methSig(m)
        //println(s"Method type : ${mType}")
        val publicFacet = auxiliaryFunctions.tBound(genVarEnv, s1.publicType)
        if (publicFacet.containsMethod(m)) {
          mType = publicFacet.methSig(m)
        }

        //check subtyping for method constraints
        if (mType.typeVars.size != types.size)
          throw TypeError(s"Method '$m' : Actual types amount must" +
            s" match the formal type variable amount")

        var extendedGenVarEnv = Helper.multiExtend(genVarEnv,mType.typeVars)
        for (pair <- mType.typeVars.zip(types)) {
          pair._1 match {
            case TypeVarSub(x, ubound) =>
              if (!subTypingAlgorithm.<::(
                genVarEnv,
                pair._2,
                auxiliaryFunctions.tBound(extendedGenVarEnv , pair._1.typeBound)))
                throw TypeError(s"Invocation of $m : Actual type for generic" +
                  s" type variable ${pair._1.typeVar} does not satisfy subtyping" +
                  s" constraint")
            case TypeVarSuper(x,lowerBound)=>
              if (!subTypingAlgorithm.<::(
                genVarEnv,
                pair._1.typeBound,
                pair._2))
                throw TypeError(s"Invocation of $m : Actual type for generic" +
                  s" type variable ${pair._1.typeVar} does not satisfy subtyping" +
                  s" constraint")
          }
        }

        //println(s"Checking subtyping between: ${args} and ${mType.domain}")
        //check the argument count
        if (mType.domain.size != args.size)
          throw TypeError(s"Method '${m}' : Actual arguments amount must" +
            s" match the formal arguments amount")
        //check subtyping between $mType.domain and s2
        for (pair <- argTypes.zip(mType.domain)) {
          val subtitutedType =closeGenType(pair._2,mType.typeVars.zip(types))
          if (!subTypingAlgorithm.<::(extendedGenVarEnv,
            pair._1,subtitutedType)) {
            throw TypeError(
              s"""Invocation of ${m}: Type ${pair._1}
             (of actual argument) is not subtyping of ${subtitutedType}""")
          }
        }

        if (publicFacet.containsMethod(m))
          closeGenType(mType.codomain,publicFacet.methSig(m).typeVars.zip(types))
        else
          closeGenType(STypeG(mType.codomain.privateType, ObjectType.top),
            privateFacet.methSig(m).typeVars.zip(types))

      }
      else
        throw TypeError(s"Method ${m} not in ${s1.privateType}")
    }
    case Obj(self, selfType, methods) =>
      val stype = closeAliases(aliasScope, selfType)
      //verify well-formed for stype
      if (!wfChecker.isWellFormed(genVarEnv,stype))
        throw CommonTypeError.secTypeIsNotWellFormed(stype.toString,
          s" ${wfChecker.errorCollector.errors}")
      //the private type must be an object type
      if (!stype.privateType.isInstanceOf[ObjectType])
        throw TypeError("The private facet must be an object type")
      //an object can not have repeated method definitions
      if (methods.map(x => x.name).distinct.size != methods.size)
        throw TypeError("An object can not have repeated method names")
      //both methods list: in type and in definition must have the same elements
      val privateMethodNames = stype.privateType.asInstanceOf[ObjectType].methods.map(x => x.name)
      val methodDefNames = methods.map(x => x.name)
      val methsNoDef = privateMethodNames.filter(x => !methodDefNames.contains(x))
      if (methsNoDef.nonEmpty) {
        throw TypeError(s"There must exist a method definition of each" +
          s" method signature. Missing method definition" +
          s" for: ${methodDefNames.foldLeft("")((acc, m) => acc + " " + m)}.")
      }
      val methsNoSignature = methodDefNames.filter(x => !privateMethodNames.contains(x))
      if (methsNoSignature.nonEmpty) {
        throw TypeError(s"There must exist a method signature for each method definition. Missing method signature for: ${methsNoSignature.foldLeft("")((acc, m) => acc + " " + m)}.")
      }
      //each method must be well-typed with respect the object type
      for (m <- methods) {
        val mType = stype.privateType.methSig(m.name)
        if (mType.domain.size != m.args.size)
          throw TypeError(s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

        val methodScope = new NestedScope[STypeG](scope)
        methodScope.add(self, stype)
        m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

        var methodGenVarEnv = Helper.multiExtend(genVarEnv,mType.typeVars)
        var s = typeCheck(methodGenVarEnv, methodScope, aliasScope, m.mBody)
        if (!subTypingAlgorithm.<::(methodGenVarEnv,s, mType.codomain))
          throw TypeError(s"Definition of method '${m.name}': " +
            s"the return type in the implementation ($s) is not subtype of " +
            s"the return type in the signature (${mType.codomain})")
      }
      stype
    case IfExpr(cond, e1, e2) =>
      val sCond = typeCheck(genVarEnv, scope, aliasScope, cond)
      if (sCond.privateType != BooleanType)
        throw TypeError("Condition of if expression must be boolean")
      val sE1 = typeCheck(genVarEnv, scope, aliasScope, e1)
      val sE2 = typeCheck(genVarEnv, scope, aliasScope, e2)
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
        td match {
          case deftype: TypeDefinition =>
            val closedType = closeAliases(newTypeAliasScope.toList(), ObjectType(deftype.name, deftype.methods))
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw TypeError(s"Type declaration '${deftype.name}' declaration " +
                s"is not well-formed: ${closedType}")
            newTypeAliasScope.add(deftype.name, closedType)
          case ta: TypeAlias =>
            val closedType = closeAliases(newTypeAliasScope.toList(), ta.objType)
            //println(newTypeAliasScope.toList())
            if (!wfChecker.isWellFormed(closedType))
              throw TypeError(s"Type in the type alias '${ta.aliasName}' " +
                s"declaration is not well-formed: ${closedType}")
            newTypeAliasScope.add(ta.aliasName, closedType)
        }
      }

      val varDeclarations = declarations
        .filter(d => d.isInstanceOf[LocalDeclaration])
        .map(ld => ld.asInstanceOf[LocalDeclaration])

      varDeclarations
        .foreach(d => letScope.
          add(d.variable,
            typeCheck(genVarEnv, letScope, newTypeAliasScope, d.rExpr)))
      typeCheck(genVarEnv, letScope, newTypeAliasScope, body)
    case ListConstructorExpr(elems) =>
      if (!elems
        .forall(e =>
          subTypingAlgorithm
            .<::(genVarEnv,typeCheck(genVarEnv, scope, aliasScope, e),
              STypeG(StringType, StringType))))
        throw TypeError("Arguments of 'mklist' must be of type String<String")
      STypeG(StringGListType(StringType), StringGListType(StringType))
    case ConsListExpr(elem, list) =>
      val tList = typeCheck(genVarEnv, scope, aliasScope, list)
      val tElem = typeCheck(genVarEnv, scope, aliasScope, elem)
      throw new NotImplementedError()
  }

  def closeAliases(aliasScope: Scope[TypeG], sType: STypeG): STypeG = {
    //TODO: Check problem with alias with same name at different scopes
    val aliases = aliasScope.toList()
    STypeG(closeAliases(aliases, sType.privateType),
      closeAliases(aliases, sType.publicType))
  }

  def closeAliases(aliases: List[(String, TypeG)],
                   theType: TypeG): TypeG = aliases match {
    case List() => theType
    case binding :: tail =>
      closeAliases(tail, TypeSubstG.substTypeVar(theType, binding._1, binding._2))
  }

  private def closeGenType(g: STypeG,
                           substitutions: List[(TypeVarSubConstraint,TypeG)]
                          ): STypeG = substitutions match {
    case List() => g
    case h::t => closeGenType(
                    STypeG(
                      TypeSubstG.substGenVar(g.privateType,h._1.typeVar,h._2),
                      TypeSubstG.substGenVar(g.publicType,h._1.typeVar,h._2)
                    ),t)

  }



}

object TypeCheckerG {
  def apply(x: ObSecGExpr): STypeG = {
    val typeChecker = new TypeCheckerG()
    typeChecker.typeCheck(x)
  }
}

trait IAuxiliaryFunctions {
  /**
    * Computes the upper bound of a type (variable) according the
    * generic type variable environment
    *
    * @param genVarEnv The generic type variable environment
    * @param theType   The type (probably a type variable)
    * @return
    */
  def tBound(genVarEnv: Environment[TypeG], theType: TypeG): TypeG
}

class AuxiliaryFunctions extends IAuxiliaryFunctions {
  /**
    * Computes the upper bound of a type (variable) according the
    * generic type variable environment
    *
    * @param genVarEnv The generic type variable environment
    * @param theType   The type (probably a type variable)
    * @return
    */
  override def tBound(genVarEnv: Environment[TypeG],
                      theType: TypeG): TypeG = theType match {
    case GenericTypeVar(x) => tBound(genVarEnv, genVarEnv.lookup(x))
    case _ => theType
  }
}