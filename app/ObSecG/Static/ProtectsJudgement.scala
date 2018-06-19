package ObSecG.Static

import Common.{Environment, ErrorCollector}
import ObSecG.Ast.{LabelG, ObjectType, TypeG}

trait SoundResult
object SoundSuccess extends SoundResult
case class SoundFailed(input:LabelG,output:LabelG) extends SoundResult

abstract class IProtectsJudgement(judgements: GObSecJudgements,
                                  errors: ErrorCollector)
  extends IJudgment(judgements,errors){
  def <<(labelVarEnvironment: LabelVarEnvironment,privateFacet:TypeG,
         publicFacet: LabelG, protectionLabel:LabelG) :Boolean

  def sound(objectType: ObjectType):SoundResult = {
    try {
      objectType.methods.forall(m => {
        val extendedEnv = judgements.auxiliaryDefinitions.multiExtend(Environment.empty(), m.mType.typeVars)
        m.mType.domain.forall(input => {
          <<(extendedEnv, input.privateType, input.publicType, m.mType.codomain.publicType)
        })
      })
      SoundSuccess
    }
    catch{
      case se:ProtectsError => new SoundFailed(se.inputLabel,se.outputLabel)
    }
  }
}

class ProtectsJudgement(judgements: GObSecJudgmentsExtensions,
                        errors: ErrorCollector)
  extends IProtectsJudgement(judgements,errors) {
  override def <<(labelVarEnvironment: LabelVarEnvironment,
                  privateFacet: TypeG,
                  publicFacet: LabelG,
                  protectionLabel: LabelG): Boolean = protectionLabel match{
    case obj@ObjectType(self,methods) =>
      methods.forall(m => {
        val extendedEnv = judgements.auxiliaryDefinitions.multiExtend(labelVarEnvironment,m.mType.typeVars)
        <<(extendedEnv,privateFacet,publicFacet,m.mType.codomain.publicType) &&
          m.mType.domain.forall(input => {
            <<(extendedEnv,input.privateType,input.publicType,m.mType.codomain.publicType)
          })
      })
    case _ =>
      if(judgements.<::(labelVarEnvironment,publicFacet,protectionLabel) == SubtypingSuccess)
        true
      else
        throw ProtectsError(publicFacet,protectionLabel)
  }
}
case class ProtectsError(inputLabel:LabelG, outputLabel:LabelG) extends  Error
