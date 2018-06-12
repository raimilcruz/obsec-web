package ObSecG.Static


import Common.{EmptyEnvironment, Environment, ErrorCollector}
import ObSecG.Ast._

abstract class IWellFormedCheckerG(judgements: GObSecJudgements, errors: ErrorCollector)
  extends IJudgment(judgements, errors){

  def isWellFormed(genVarEnv: LabelVarEnvironment, stype: STypeG): Boolean

  def isWellFormed(stype: STypeG): Boolean
}

/**
  * Implements well-formed judgments for types.
  */
class WellFormedCheckerG(judgements: GObSecJudgements, errorCollector: ErrorCollector)
  extends IWellFormedCheckerG(judgements,errorCollector) {

  def isWellFormed(stype: STypeG): Boolean =
    //isWellFormed(stype.privateType) && isWellFormed(stype.publicType)
  throw new NotImplementedError()

  def isWellFormed(genVarEnv: LabelVarEnvironment,
                   stype: STypeG): Boolean =
    isWellFormed(genVarEnv, stype.privateType) &&
     isWellFormed(genVarEnv, stype.publicType)

  def isWellFormed(labelVariableEnv: LabelVarEnvironment, t: LabelG): Boolean =
    isWellFormed(labelVariableEnv, Environment.empty[ObjectType](), t)


  def isWellFormed(t: LabelG): Boolean =
    isWellFormed(Environment.empty[TypeVarBounds](),Environment.empty[ObjectType](), t)

  private def isWellFormed(genVarEnv: LabelVarEnvironment,
                           objectEnv: SelfDefinitionEnvironment,
                           s: STypeG): Boolean =
    isWellFormed(genVarEnv,objectEnv,s.privateType) &&
      isWellFormed(genVarEnv,objectEnv,s.publicType)

   private def isWellFormed(genVarEnv: LabelVarEnvironment,
                             objectEnv: SelfDefinitionEnvironment,
                             t: LabelG): Boolean = t match {
      case p: PrimType => true
      case Bottom => true
      case TypeVar(x) => true
      case x:LabelVar => true
      case obj@ObjectType(x, methods) =>
        val newEnv = Environment.extend(objectEnv, x, obj)
          methods
            .forall(m => {
              val methodLabelEnvironment = judgements.
                auxiliaryDefinitions.
                multiExtend(genVarEnv, m.mType.typeVars)
              labelVariableWellFormed(genVarEnv,objectEnv,m.mType.typeVars) &&
              m.mType
                .domain
                .forall(s =>
                  isWellFormed(methodLabelEnvironment,newEnv,s)) &&
                isWellFormed(
                  methodLabelEnvironment,newEnv, m.mType.codomain)
            })
    }

  private def labelVariableWellFormed(genVarEnv: LabelVarEnvironment,
                                     objectEnv: SelfDefinitionEnvironment,
                                     typeVars: List[BoundedLabelVar]):Boolean= typeVars match{
    case List() => true
    case h::tail =>
      if(!isWellFormed(genVarEnv,objectEnv,h.upperBound)){
        errorCollector.report("Type bound is not well-formed")
        return false
      }
      if(!isWellFormed(genVarEnv,objectEnv,h.lowerBound)){
        errorCollector.report("Type bound is not well-formed")
        return false
      }
      labelVariableWellFormed(
        Environment.extend(genVarEnv,h.typeVar,h.bounds)
        ,objectEnv,tail)
  }
  /*
     def closeType(env: Environment[ObjectType], t: TypeG): TypeG =
       if (env.isEmpty) t
       else {
         val head = env.head
         closeType(env.tail, TypeSubstG.substTypeVar(t, head._1, head._2))
       }

     private def isWellFormed(genVarEnv: Environment[TypeG],
                              delta: Delta,
                              env: Environment[ObjectType],
                              s: STypeG): Boolean =
       isWellFormed(genVarEnv, delta, env, s.privateType) &&
         isWellFormed(genVarEnv, delta, env, s.publicType) &&
         (sb.<::(
           genVarEnv,
           closeType(env, s.privateType),
           closeType(env, s.publicType)) || {
           errorCollector.report(s"Private facet must be subtype of " +
             s"public facet in security type: ${s}")
           false
         })

    */
}
