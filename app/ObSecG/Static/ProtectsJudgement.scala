package ObSecG.Static

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._

trait SoundResult
object SoundSuccess extends SoundResult
case class SoundFailed(input:LabelG,output:LabelG) extends SoundResult

abstract class IProtectsJudgement(judgements: GObSecJudgements,
                                  errors: ErrorCollector)
  extends IJudgment(judgements,errors){
  def <<(labelVarEnvironment: LabelVarEnvironment,privateFacet:TypeG,
         publicFacet: LabelG, protectionLabel:LabelG) :Boolean

  def sound(x:Any):SoundResult = {
   throw new NotImplementedError("IProtectsJudgement.sound not implemented")
  }
  /*

  def sound(map: Map[PrimADT,SoundResult],primADT:PrimADT, objectType: ObjectType):SoundResult = {
    if(map.contains(primADT))
      map(primADT)
    else{
      try {
        var extendedMap = map + (primADT -> SoundSuccess)
        //we have to verify:
        //1. all method in the object types are in the primADT
        //2. Inner private types are sound
        //3. Inner public types respect the protects rules

        if(primADT.operators.size != objectType.methods.size)
          throw new Error("Primitive ADTs and lifted object must have the same methods")
        for(operator <- primADT.operators){
          val res = objectType.methods.find(m=>m.name==operator.name)
          res match {
            case None => throw new Error("All methods in primitive ADTs must be in lifted object")
            case Some(m) =>
              if(m.mType.domain.size != operator.domain.size)
                throw new Error("Lifted object type does match primitive operator argument size")

              val extendedEnv = judgements.auxiliaryDefinitions.multiExtend(Environment.empty(), m.mType.typeVars)
              sound(extendedMap,operator.codomain,m.mType.codomain.privateType.asInstanceOf[ObjectType])
              operator.domain.zip(m.mType.domain).forall(pair => {
                  <<(extendedEnv, pair._2.privateType, pair._2.publicType, m.mType.codomain.publicType) &&
                    sound(extendedMap,pair._1 ,pair._2.privateType.asInstanceOf[ObjectType]) == SoundSuccess

              })
          }
        }
        SoundSuccess
      }
      catch{
        case se:ProtectsError => SoundFailed(se.inputLabel, se.outputLabel)
      }
    }
  }*/
}

class ProtectsJudgement(judgements: GObSecJudgmentsExtensions,
                        errors: ErrorCollector)
  extends IProtectsJudgement(judgements,errors) {
  override def <<(labelVarEnvironment: LabelVarEnvironment,
                  privateFacet: TypeG,
                  publicFacet: LabelG,
                  protectionLabel: LabelG): Boolean = protectionLabel match{

    //Prot-Obj
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

object ProtectsJudgement{
  def isSoundMethodSignature(mt:MTypeG): Boolean ={
    isSound(mt) || isImplicitSignature(mt)
  }
  private def isSound(mt:MTypeG)={
    if(mt.domain.forall(x=> x.publicType == x.privateType))
      true
    else
      TypeEquivalenceG.alphaEq(mt.codomain.publicType,ObjectType.top)
  }
  private def isImplicitSignature(mt: MTypeG): Boolean ={
    mt.domain.forall(x=>x.publicType == ImplicitLabel) &&
      mt.codomain.publicType == ImplicitLabel
  }
}
case class ProtectsError(inputLabel:LabelG, outputLabel:LabelG) extends  Error
