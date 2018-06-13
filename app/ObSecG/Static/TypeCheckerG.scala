package ObSecG.Static

import ObSecG.Ast._
import Common._


abstract class ITypeChecker(judgements: GObSecJudgements,
                            errorCollector: ErrorCollector)
  extends IJudgment(judgements, errorCollector) {

  def typeCheck(x: ObSecGExpr): STypeG

  def typeCheck(labelVarEnv: LabelVarEnvironment,
                scope: TypeEnvironment,
                aliasScope: TypeAliasScope,
                expr: ObSecGExpr): STypeG
}

/**
  * Created by racruz on 24-08-2017.
  */
class TypeChecker(judgements: GObSecJudgmentsExtensions,
                  errorCollector: ErrorCollector)
  extends ITypeChecker(judgements: GObSecJudgements, errorCollector) {

  private val auxiliaryFunctions = judgements.auxiliaryDefinitions

  def typeCheck(x: ObSecGExpr): STypeG =
    typeCheck(Environment.empty(), new Scope, new Scope, x)


  /**
    *
    * @param genVarEnv  The generic variable subtyping constraint environment(eg::= X<:T,..)
    * @param scope      The value environment
    * @param aliasScope The environment for type aliases
    * @param expr       The expression
    * @return
    */
  def typeCheck(genVarEnv: LabelVarEnvironment,
                scope: TypeEnvironment,
                aliasScope: TypeAliasScope,
                expr: ObSecGExpr): STypeG = expr match {
    case Var(x) => scope.lookup(x)
    case IntExpr(_) => STypeG(IntType, IntType)
    case BooleanExpr(_) => STypeG(BooleanType, BooleanType)
    case StringExpr(_) => STypeG(StringType, StringType)
    case methInv@MethodInv(e1, types, args, m) => {
      val s1 = typeCheck(genVarEnv, scope, aliasScope, e1)
      val argTypes = args.map(param =>
        typeCheck(genVarEnv, scope, aliasScope, param))
      //println(s"Args: ${argTypes}")
      //facet analysis
      //get the upper bound of both types
      val privateFacet = auxiliaryFunctions.tUpperBound(genVarEnv, s1.privateType)
      //println("typeCheck:" + s"private facet:$privateFacet")
      if (privateFacet.containsMethod(m)) {
        var mType = privateFacet.methSig(m)
        //println(s"Method type : ${mType}")
        val publicFacet = auxiliaryFunctions.tUpperBound(genVarEnv, s1.publicType)
        if (publicFacet.containsMethod(m)) {
          mType = publicFacet.methSig(m)
        }

        //check subtyping for method constraints
        print(s"mType.typeVars.size != types.size. typevars: ${mType.typeVars}   types: $types")
        if (mType.typeVars.size != types.size)
          throw TypeErrorG.actualTypeParametersMustMatchFormalTypeParameterAmount(expr.astNode,m)

        //actual type arguments are good for formal type arguments
        var extendedGenVarEnv = genVarEnv
        var substitutions = List[(BoundedLabelVar,LabelG)]()
        for (pair <- mType.typeVars.zip(types)) {
          //TODO: finish
          val closedLowerBound =closeGenType(pair._1.lowerBound,substitutions)
          val closeUpperBound = closeGenType(pair._1.upperBound,substitutions)
          extendedGenVarEnv = genVarEnv.extend(
            pair._1.typeVar,
            TypeVarBounds(closedLowerBound,closedLowerBound))
          if(!typeInBound(extendedGenVarEnv,pair._2,closedLowerBound,closeUpperBound))
            throw TypeErrorG.badActualLabelArgument(e1.astNode, m, pair._1.typeVar)
          substitutions = substitutions ++ List((pair._1,pair._2))
        }
        //check the argument count
        if (mType.domain.size != args.size)
          throw TypeErrorG.actualArgumentsSizeError(methInv)
        //check subtyping between $mType.domain and s2
        for (pair <- argTypes.zip(mType.domain)) {
          val subtitutedType =
            closeGenType(
              pair._2,
              mType.typeVars.zip(types))
          if (!judgements.<::(extendedGenVarEnv,
            pair._1, subtitutedType)) {
            throw TypeErrorG.subTypingError(expr.astNode, m,pair._1,subtitutedType)
          }
        }

        if (publicFacet.containsMethod(m))
          closeGenType(mType.codomain,
            publicFacet.methSig(m).typeVars.zip(types))
        else
          closeGenType(STypeG(mType.codomain.privateType, UnionLabel(mType.codomain.publicType, s1.publicType)),
            privateFacet.methSig(m).typeVars.zip(types))

      }
      else
        throw TypeErrorG.methodNotFound(expr.astNode,m)
    }
    case Obj(self, selfType, methods) =>
      val stype = closeAliases(aliasScope, selfType)
      //verify well-formed for stype
      if (!judgements.isWellFormed(genVarEnv, stype))
        throw CommonTypeError.secTypeIsNotWellFormed(stype.toString,
          s" ${errorCollector.errors}")
      //the private type must be an object type
      //TODO: check this in the ResolverIdentifier
      if (!stype.privateType.isInstanceOf[ObjectType])
        throw TypeError("The private facet must be an object type")
      //1. An object can not have repeated method definitions. We check for repeated method
      // in the ResolverIdentifier
      //both methods list: in type and in definition must have the same elements
      //TODO: Check this in the ResolverIdentifier
      val privateMethodNames = stype.privateType.asInstanceOf[ObjectType].methods.map(x => x.name)
      val methodDefNames = methods.map(x => x.name)
      val methsNoDef = privateMethodNames.filter(x => !methodDefNames.contains(x))
      if (methsNoDef.nonEmpty) {
        throw TypeError(s"There must exist a method definition of each" +
          s" method signature. Missing method definition" +
          s" for: ${methodDefNames.foldLeft("")((acc, m) => acc + " " + m)}.")
      }
      //TODO: Check this in the ResolverIdentifier
      val methsNoSignature = methodDefNames.filter(x => !privateMethodNames.contains(x))
      if (methsNoSignature.nonEmpty) {
        throw TypeError(s"There must exist a method signature for each method definition. Missing method signature for: ${methsNoSignature.foldLeft("")((acc, m) => acc + " " + m)}.")
      }
      //each method must be well-typed with respect the object type
      for (m <- methods) {
        val mType = stype.privateType.methSig(m.name)
        //TODO: Check this in the ResolverIdentifier
        if (mType.domain.size != m.args.size)
          throw TypeError(s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

        val methodScope = new NestedScope[STypeG](scope)
        methodScope.add(self, stype)
        m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

        var methodGenVarEnv = auxiliaryFunctions.multiExtend(genVarEnv, mType.typeVars)
        var s = typeCheck(methodGenVarEnv, methodScope, aliasScope, m.mBody)
        print(s"$s <:: ${mType.codomain}")
        if (!judgements.<::(methodGenVarEnv, s, mType.codomain))
          throw TypeErrorG.returnTypeError(m.mBody.astNode,m.name,s.toString,mType.codomain.toString)
      }
      stype
    case IfExpr(cond, e1, e2) =>
      val sCond = typeCheck(genVarEnv, scope, aliasScope, cond)
      if (sCond.privateType != BooleanType)
        throw TypeErrorG.ifConditionExpectABoolean(cond.astNode)
      val sE1 = typeCheck(genVarEnv, scope, aliasScope, e1)
      val sE2 = typeCheck(genVarEnv, scope, aliasScope, e2)
      if (sE1 != sE2)
        throw TypeErrorG.sameTypeForIfBranches(expr.astNode)
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
            val closedType = closeAliases(
              newTypeAliasScope.toList,
              ObjectType(deftype.name, deftype.methods))
            //println(newTypeAliasScope.toList())
            if (!judgements.isWellFormed(Environment.empty():LabelVarEnvironment, closedType))
              throw TypeError(s"Type declaration '${deftype.name}' declaration " +
                s"is not well-formed: $closedType")
            newTypeAliasScope.add(deftype.name, closedType.asInstanceOf[TypeG])
          case ta: TypeAlias =>
            val closedType = closeAliases(newTypeAliasScope.toList, ta.objType)
            //println(newTypeAliasScope.toList())
            if (!judgements.isWellFormed(Environment.empty():LabelVarEnvironment, closedType))
              throw TypeError(s"Type in the type alias '${ta.aliasName}' " +
                s"declaration is not well-formed: $closedType")
            newTypeAliasScope.add(ta.aliasName, closedType.asInstanceOf[TypeG])
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
          judgements
            .<::(genVarEnv, typeCheck(genVarEnv, scope, aliasScope, e),
              STypeG(StringType, StringType))))
        throw TypeError("Arguments of 'mklist' must be of type String<String")
      STypeG(StringGListType(StringType), StringGListType(StringType))
    case ConsListExpr(elem, list) =>
      val tList = typeCheck(genVarEnv, scope, aliasScope, list)
      val tElem = typeCheck(genVarEnv, scope, aliasScope, elem)
      throw new NotImplementedError()
  }

  private def typeInBound(genVarEnv: LabelVarEnvironment,
                          actualType:LabelG,
                          lower: LabelG, upper:LabelG):Boolean = {
     judgements.<::(genVarEnv, actualType, upper) &&
       judgements.<::(genVarEnv, lower, actualType)
  }

  def closeAliases(aliasScope: TypeAliasScope, sType: STypeG): STypeG = {
    //TODO: Check problem with alias with same name at different scopes
    val aliases = aliasScope.toList
    STypeG(closeAliases(aliases, sType.privateType).asInstanceOf[TypeG],
      closeAliases(aliases, sType.publicType))
  }

  def closeAliases(aliases: List[(String, TypeG)],
                   theType: LabelG): LabelG = aliases match {
    case List() => theType
    case binding :: tail =>
      closeAliases(tail, TypeSubstG.substRecVar(theType, binding._1, binding._2))
  }

  private def closeGenType(g: STypeG,
                           substitutions: List[(BoundedLabelVar, LabelG)]
                          ): STypeG = substitutions match {
    case List() => g
    case (tv,actualLabel) :: t => closeGenType(
      STypeG(
        TypeSubstG.substLabel(g.privateType, tv, actualLabel).asInstanceOf[TypeG],
        TypeSubstG.substLabel(g.publicType, tv, actualLabel)
      ), t)

  }
  private def closeGenType(openType: LabelG,
                           substitutions: List[(BoundedLabelVar, LabelG)]
                          ): LabelG = substitutions match {
    case List() => openType
    case (tv,actualLabel) :: t =>
      closeGenType(TypeSubstG.substLabel(openType, tv, actualLabel), t)

  }
}

object TypeCheckerG {
  def apply(x: ObSecGExpr): STypeG = {
    val judgmentsImpl = new GObSecGJudgmentImpl(new ErrorCollector)
    judgmentsImpl.typeCheck(
      Environment.empty(),
      new Scope,
      new Scope,
      x)
  }
}

