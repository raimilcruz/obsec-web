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
    isWellFormed(genVarEnv,Environment.empty[ObjectType](),stype)

  def isWellFormed(labelVariableEnv: LabelVarEnvironment, t: LabelG): Boolean =
    isWellFormed(labelVariableEnv, Environment.empty[ObjectType](), t)


  def isWellFormed(t: LabelG): Boolean =
    isWellFormed(Environment.empty[TypeVarBounds](),Environment.empty[ObjectType](), t)

  private def isWellFormed(genVarEnv: LabelVarEnvironment,
                           objectEnv: SelfDefinitionEnvironment,
                           s: STypeG): Boolean =
    isWellFormed(genVarEnv,objectEnv,s.privateType) &&
      isWellFormed(genVarEnv,objectEnv,s.publicType) &&
      (
        /*if(s.publicType == ImplicitLabel)
          true
        else*/
        ( judgements.<::(genVarEnv, closeType(objectEnv,s.privateType),closeType(objectEnv,s.publicType)) == SubtypingSuccess
          || {
          errorCollector.report(s"Private facet must be subtype of public facet in security type: ${s.astNode.source}")
          false
          }
        )
      )

   private def isWellFormed(genVarEnv: LabelVarEnvironment,
                             objectEnv: SelfDefinitionEnvironment,
                             t: LabelG): Boolean = t match {
      case p: PrimType => true
      case ImplicitLabel => true
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
              //verify that if signature uses implict labels, then all label need to be implicit
                (if(m.mType.usedImplicitLabels)
                  m.mType.computedIsPrimitive
                else
                  m.mType
                    .domain
                    .forall(s =>
                      isWellFormed(methodLabelEnvironment,newEnv,s)) &&
                    isWellFormed(
                      methodLabelEnvironment,newEnv, m.mType.codomain))
            })
     /* case UnionLabel(left,right)=>
        isWellFormed(genVarEnv,objectEnv,left) &&
          isWellFormed(genVarEnv,objectEnv,right)*/
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

   private def closeType(env: Environment[ObjectType], t: LabelG): LabelG =
     if (env.isEmpty) t
     else {
       val head = env.head
       closeType(env.tail, TypeSubstG.substRecVar(t, head._1, head._2))
     }
}
