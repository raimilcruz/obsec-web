package ObSecE.Static

import ObSecE.Ast._
import Common._

import scala.collection.mutable

abstract class IEObSecTypeChecker(judgements: EObSecJudgements,
                                   errorCollector: ErrorCollector){
  def typeCheck(x: EObSecExpr): STypeE
}
class EObSecTypeChecker(judgements: EObSecJudgements,
                        errorCollector: ErrorCollector)
  extends IEObSecTypeChecker(judgements,errorCollector) {

  private val auxiliaryFunctions = judgements.auxiliaryDefinitions

  def typeCheck(x: EObSecExpr): STypeE =
    typeCheck(Environment.empty(), new Scope, new Scope, x)



  def typeCheck(deltaInst: Environment[TypeE], //X:I,...
                scope:  Scope[STypeE],
                aliasScope:  Scope[LabelE],
                expr: EObSecExpr): STypeE =
  //wrapError[STypeE](
    expr match {
      case Var(x) => scope.lookup(x)
      case p:PrimitiveLiteral => typePrimitives(p)
      case methInv@MethodInv(e1, args, m) => {
        val s1 = typeCheck(deltaInst, scope, aliasScope, e1)
        val argTypes = args.elems.map(param =>
          typeCheck(deltaInst, scope, aliasScope, param).setAstNode(param.astNode))

        val resolvedPrivateFacet = EObSecTypeChecker.resolveDirectTypeId(aliasScope,s1.privateType)
        if(auxiliaryFunctions.mInU(m,resolvedPrivateFacet)){
          //case for object types
          //case for primitive types
          var mType = auxiliaryFunctions.mSig(resolvedPrivateFacet,m)
          //println(s"Method type : ${mType}")
          val resolvedPublicFacet = EObSecTypeChecker.resolveDirectTypeId(aliasScope,s1.publicType)
          if (auxiliaryFunctions.mInU(m,resolvedPublicFacet)) {
            mType = auxiliaryFunctions.mSig(resolvedPublicFacet,m)
          }

          //instantiate method signature with actual type argument and arguments types
          val closedSignature =  mType
          //check the argument count
          if (closedSignature.domain.size != args.elems.size)
            throw CommonError.actualArgumentsSizeError(methInv.args.astNode,methInv.method)
          //check subtyping between $mType.domain and s2
          for ((actualT,formalT) <- argTypes.zip(closedSignature.domain)) {
            if (judgements.<::(aliasScope,deltaInst,actualT, formalT).isInstanceOf[SubtypingFail[STypeE]]) {
              throw CommonError.subTypingError(args.astNode, m, actualT.prettyPrint(),formalT.prettyPrint())
            }
          }

          if (auxiliaryFunctions.mInU(m,resolvedPublicFacet))
            closedSignature.codomain
          else
            FTypeE(closedSignature.codomain.privateType,ObjectType.top)
        }
        else
          throw CommonError.methodNotFound(methInv.methodNameNode, m)
      }
      case Obj(self, selfType, methods) =>
        val stype =selfType
        //val stype = closeAliases(aliasScope, selfType,labelSubstitution)
        //verify well-formed for stype
        if (!judgements.isWellFormed(aliasScope,deltaInst, selfType))
          throw CommonError.secTypeIsNotWellFormed(stype.toString,
            s" ${errorCollector.errors}",stype.astNode)
        //the private type must be an object type
        //TODO: check this in the ResolverIdentifier
        val resolvedPrivateType = EObSecTypeChecker.resolveDirectTypeId(aliasScope,stype.privateType).asInstanceOf[ObjectType]
        if (resolvedPrivateType == null)
          throw CommonError.genericError(selfType.astNode,"The private facet must be an object type")
        //1. An object can not have repeated method definitions. We check for repeated method
        // in the ResolverIdentifier
        //both methods list: in type and in definition must have the same elements
        //TODO: Check this in the ResolverIdentifier
        val privateMethodNames = resolvedPrivateType.methods.map(x => x.name)
        val methodDefNames = methods.map(x => x.name)
        val methsNoDef = privateMethodNames.filter(x => !methodDefNames.contains(x))
        if (methsNoDef.nonEmpty) {
          throw CommonError.missingMethodDefinition(expr.astNode, methodDefNames)
        }
        //TODO: Check this in the ResolverIdentifier
        val methsNoSignature = methodDefNames.filter(x => !privateMethodNames.contains(x))
        if (methsNoSignature.nonEmpty) {
          throw CommonError.genericError(expr.astNode,s"There must exist a method signature for each method definition. Missing method signature for: ${methsNoSignature.foldLeft("")((acc, m) => acc + " " + m)}.")
        }
        //each method must be well-typed with respect the object type
        for (m <- methods) {
          val mType = auxiliaryFunctions.mSig(resolvedPrivateType,m.name)
          //TODO: Check this in the ResolverIdentifier
          if (mType.domain.size != m.args.size)
            throw CommonError.genericError(m.astNode,s"Method '${m.name}': Mismatch in amount of arguments between definition and signature")

          val methodScope = new NestedScope[STypeE](scope)

          methodScope.add(self, stype)
          m.args.zip(mType.domain).foreach(a => methodScope.add(a._1, a._2))

          val extendDeltaInst =  EObSecTypeChecker.extendDeltaInstWithExistentialVariables(aliasScope,deltaInst,mType.domain)

          var s = typeCheck(extendDeltaInst, methodScope, aliasScope, m.mBody)
          //print(s"$s <:: ${mType.codomain}")
          if (judgements.<::(aliasScope, extendDeltaInst, s, mType.codomain).isInstanceOf[SubtypingFail[STypeE]])
            throw CommonError.returnTypeError(m.mBody.astNode, m.name, s.prettyPrint(), mType.codomain.prettyPrint())
        }
        {
          println("Type checked object")
          stype
        }
      case IfExpr(cond, e1, e2) =>
        val sCond = typeCheck(deltaInst, scope, aliasScope, cond)
        if (sCond.privateType != BoolADT)
          throw CommonError.ifConditionExpectABoolean(cond.astNode)
        val sE1 = typeCheck(deltaInst, scope, aliasScope, e1)
        val sE2 = typeCheck(deltaInst, scope, aliasScope, e2)
        if (sE1 != sE2)
          throw CommonError.sameTypeForIfBranches(expr.astNode,sE1.prettyPrint(),sE2.prettyPrint())
        //depending on the type of the condition we should lift the public type of the resulting type
        //TODO: Implement this properly: I should check for the empty object type
        if (sCond.publicType != BoolADT)
          FTypeE(sE1.privateType, ObjectType.top)
        else sE1
      case LetStarExpr(declarations, body) =>
        val newTypeAliasScope = new NestedScope[LabelE](aliasScope)
        val letScope = new NestedScope[STypeE](scope)

        val typeDefs = declarations.filter(d => d.isInstanceOf[TypeDefinition] || d.isInstanceOf[TypeAlias])
        for (td <- typeDefs) {
          td match {
            case deftype@TypeDefinition(name,methods) =>
              val syntheticObjectType = ObjectType(name,methods)
              /*val closedType = closeAliases(
                newTypeAliasScope.toList,
                ObjectType(deftype.name, deftype.methods),mutable.HashMap[String,String]())*/
              //println(newTypeAliasScope.toList())
              if (!judgements.isWellFormed(newTypeAliasScope,Environment.empty[TypeE](),syntheticObjectType))
                throw CommonError.namedTypeIsNotWellFormed(deftype.astNode,deftype.name,errorCollector.errors)
              newTypeAliasScope.add(deftype.name, syntheticObjectType.asInstanceOf[TypeE])
            case ta@TypeAlias(name,namedType) =>
              //val closedType = closeAliases(newTypeAliasScope.toList, ta.objType,mutable.HashMap[String,String]())
              //println(newTypeAliasScope.toList())
              if (!judgements.isWellFormed(newTypeAliasScope, Environment.empty[TypeE](), namedType))
                throw CommonError.namedTypeIsNotWellFormed(ta.astNode,ta.aliasName,errorCollector.errors)
              newTypeAliasScope.add(ta.aliasName, namedType)
          }
        }

        val varDeclarations = declarations
          .filter(d => d.isInstanceOf[LocalDeclaration])
          .map(ld => ld.asInstanceOf[LocalDeclaration])

        varDeclarations
          .foreach(d => letScope.
            add(d.variable,
              typeCheck(deltaInst, letScope, newTypeAliasScope, d.rExpr)))
        typeCheck(deltaInst, letScope, newTypeAliasScope, body)
    }//,expr.astNode)


  /*def closeAliases(aliasScope: Scope[LabelE], sType: STypeE,
                   labelSubstitution: mutable.HashMap[String,String]): STypeE = {
    //TODO: Check problem with alias with same name at different scopes
    val aliases = aliasScope.toList
    sType match {
      case FTypeE(priv,pub)=>
        FTypeE(closeAliases(aliases, priv,labelSubstitution).asInstanceOf[TypeE],closeAliases(aliases,pub,labelSubstitution))
      case ESTypeE(priv,impl,existential)=> ???
    }
  }
  def closeAliases(aliases: List[(String, LabelE)],
                   theType: LabelE,labelSubstitution: mutable.HashMap[String,String]): LabelE = aliases match {
    case List() => theType
    case binding :: tail =>
      closeAliases(tail, auxiliaryFunctions.subst(theType, binding._1, binding._2,labelSubstitution),labelSubstitution)
  }*/

  def typePrimitives(expr: PrimitiveLiteral):FTypeE = expr match{
    case IntExpr(_) => FTypeE(IntADT, IntADT)
    case BooleanExpr(_) => FTypeE(BoolADT, BoolADT)
    case StringExpr(_) => FTypeE(StringADT, StringADT)
  }


  private def renameLabels(e: EObSecExpr,labelSubstitution:mutable.HashMap[String,String]):EObSecExpr=  (e match {
    case ot@Obj(selfName,selfAscription,methods) =>
      Obj(selfName,TypeSubstE.renameLabels(selfAscription,labelSubstitution.toMap).setAstNode(selfAscription.astNode),methods)
    case methInv@MethodInv(e1, args, m) =>
      MethodInv(renameLabels(e1,labelSubstitution),
        NodeList(args.map(a=>renameLabels(a,labelSubstitution)).toList).setAstNode(args.astNode),m)
    case LetStarExpr(declarations, body) =>
     LetStarExpr(declarations.map {
       case ld@LocalDeclaration(name, objExpr) =>
         LocalDeclaration(name, renameLabels(objExpr, labelSubstitution)).setAstNode(ld.astNode)
       case decl => decl
     },renameLabels(body,labelSubstitution))
    case IfExpr(cond,thenExp,elseExpr)=>
      IfExpr(renameLabels(cond,labelSubstitution),
        renameLabels(thenExp,labelSubstitution),
        renameLabels(elseExpr,labelSubstitution))
    case _ => e
  }).setAstNode(e.astNode)

}

object EObSecTypeChecker {


  def apply(x: EObSecExpr): STypeE = {
    val errorCollector = new ErrorCollector()
    val judgments = new EObSecGJudgmentImpl(errorCollector)
    new EObSecTypeChecker(judgments, errorCollector).typeCheck(x)
  }



  def extendExistEnvWithExistentialVariables(aliasScope:Scope[LabelE], eVarEnv: Environment[TypeE],
                                                t: STypeE): Environment[TypeE] = t match{
    case ESTypeE(priv,impl,existentialFacet) =>
      resolveDirectTypeId(aliasScope,existentialFacet) match{
        case ExistentialType(typeVars,methods)=>
          typeVars.foldLeft(eVarEnv)(
            (acc,existentialVar) =>
              acc.extend(existentialVar.name,existentialVar.lowerBound))
        case _ => eVarEnv
      }
    case _ => eVarEnv
  }

  def extendDeltaInstWithExistentialVariables(aliasScope:Scope[LabelE],deltaInst: Environment[TypeE], types: List[STypeE]): Environment[TypeE] =
    types.foldLeft(deltaInst)(
      (acc,argType)=>  extendDeltaInstWithExistentialVariables(aliasScope,acc,argType))

  def extendDeltaInstWithExistentialVariables(aliasScope:Scope[LabelE],
                                              eVarEnv: Environment[TypeE],
                                              t: STypeE): Environment[TypeE] = t match{
    case ESTypeE(priv,impl,existentialFacet) =>
      resolveDirectTypeId(aliasScope,existentialFacet) match{
        case ExistentialType(typeVars,methods)=>
          typeVars.zip(impl).foldLeft(eVarEnv)(
            (acc,existentialVar) =>
              acc.extend(existentialVar._1.name,existentialVar._2))
        case _ => eVarEnv
      }
    case _ => eVarEnv
  }
  def resolveDirectTypeId(aliasScope: Scope[LabelE], theType: LabelE): LabelE = theType match{
    case TypeId(name) => aliasScope.lookup(name)
    case _ => theType
  }
}

