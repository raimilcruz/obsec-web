package ObSecE.Static

import Common._
import ObSecE.Ast._

import scala.collection.mutable


abstract class IEObSecSubtyping(judgements: EObSecJudgements,
                                 errors: ErrorCollector)
  extends IJudgment(judgements: EObSecJudgements, errors: ErrorCollector){

  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE],t1: LabelE, t2: LabelE): SubtypingResult
  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE],s1:STypeE,s2:STypeE):SubtypingResult
}


class AmadioCardelliSubtypingEObsec(judgements: EObSecJudgements,
                                errors: ErrorCollector)
  extends IEObSecSubtyping(judgements: EObSecJudgements,errors: ErrorCollector) {

  private val auxiliaryFunctions = judgements.auxiliaryDefinitions

  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE],t1: LabelE, t2: LabelE): SubtypingResult =
    try {
      //print("######### Subtyping root judgment ##########")
      val subtypingAssumptions = deltaInst
        .toList
        .foldLeft(Set[(LabelE, LabelE)]())(
          (prevSet,tv)=>prevSet + Tuple2(tv._2,LabelVar(tv._1)))
      val res = innerSubType(typeAliasScope,deltaInst,subtypingAssumptions, t1, t2,0)
      SubtypingSuccess
    } catch {
      case x: SubtypingError => SubtypingFail(x.t1,x.t2).setMessage(x.message)
    }

  def <::(typeAliasScope:Scope[LabelE],deltaIns:Environment[TypeE],s1: STypeE, s2: STypeE): SubtypingResult = (s1,s2) match {
    case (FTypeE(priv1, pub1), FTypeE(priv2, pub2)) =>
      <::(typeAliasScope, deltaIns, priv1, priv2) &&
        <::(typeAliasScope, deltaIns, pub1, pub2)
    case (ESTypeE(priv1,implTypes1, pub1), ESTypeE(priv2,implType2, pub2))=>
       if(implTypes1.size != implType2.size)
         SubtypingFail(s1,s2).setMessage("The existential faceted types do not have different amount of implementation types")
       else
          <::(typeAliasScope, deltaIns, priv1, priv2) &&
            implTypes1.zip(implType2).foldLeft[SubtypingResult](SubtypingSuccess)(
              (acc,p)=> acc && <::(typeAliasScope,deltaIns,p._1,p._2)
            ) &&
            <::(typeAliasScope, deltaIns, pub1, pub2)
    case (FTypeE(priv1, pub1), ESTypeE(priv2,implType, pub2)) =>
      <::(typeAliasScope, deltaIns, pub1, priv2)
  }

  private def <::(typeAliasScope:Scope[LabelE],
                  deltaInst:Environment[TypeE],
                  alreadySeen: Set[(LabelE, LabelE)],
                  s1: STypeE,
                  s2: STypeE,
                  deep:Int): Set[(LabelE, LabelE)] = {
    val set = innerSubType(typeAliasScope,deltaInst,alreadySeen, s1.publicType, s2.publicType,deep)
    innerSubType(typeAliasScope,deltaInst,set, s1.privateType, s2.privateType,deep)
  }


  private def innerSubType(typeAliasScope:Scope[LabelE],
                            deltaInst: Environment[TypeE],
                           alreadySeen: Set[(LabelE, LabelE)],
                           t1: LabelE,
                           t2: LabelE,
                           deep:Int): Set[(LabelE, LabelE)]= {

    val printRules = true
    val spaces = (1 to deep).foldLeft("")((acc,x)=>acc+" ")
    /*println(spaces + " **********************")
    println(spaces + deep)
    println(s"$spaces Label env: ${delta.prettyPrint}")
    println(s"$spaces Already seen: ${alreadySeen.map(p => p._1.prettyPrint() +"<:"+ p._2.prettyPrint()).mkString(";")}")

    println(s"$spaces Goal: ${t1.prettyPrint()} and ${t2.prettyPrint()}")
    println(spaces + " *********************")*/

    if (alreadySeen.exists((x) => EObSecTypeEquivalence.alphaEq(x._1, t1) &&
      EObSecTypeEquivalence.alphaEq(x._2, t2)))
      alreadySeen
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      //println("In subtyping rules")
      (t1, t2) match {
        // T <: Top
        case (_, t) if EObSecTypeEquivalence.alphaEq(t, ObjectType.top) =>
          if(printRules) println(s"$spaces [TypeEq]")
          newSet
        //little optimization
        case (_,_) if EObSecTypeEquivalence.alphaEq(t1,t2) => newSet
        case (TypeId(tId),_) =>
          innerSubType(typeAliasScope,deltaInst,newSet,typeAliasScope.lookup(tId) ,t2,deep+1)
        case (_,TypeId(tId)) =>
          innerSubType(typeAliasScope,deltaInst,newSet,t1 ,typeAliasScope.lookup(tId),deep+1)
        case (t, gv2: LabelVar) =>
          if(printRules) println(s"$spaces [LabelR]")

          val implType = deltaInst.lookup(gv2.name)
          innerSubType(typeAliasScope,
            deltaInst,
            newSet,
            t,
            implType, deep + 1)
        case (RecordTypeE(methodsR1), RecordTypeE(methodsR2)) =>
          if(printRules) println(s"$spaces [Record]")

          var set = newSet
          for (m2 <- methodsR2) {
            val mt2 = m2.mType
            val m1Option = methodsR1.find(x => x.name == m2.name)
            if(m1Option.isEmpty)
              throw SubtypingError(t1,t2).setMessage(s"Method ${m2.name} of " +
                s"second type is not in the first type")
            val mt1 = m1Option.get.mType
            var extendedGenVarEnv = deltaInst

            for (pair <- mt2.domain.zip(mt1.domain)) {
              set = <::(typeAliasScope,extendedGenVarEnv,set, pair._1, pair._2,deep+1)
            }
            set = <::(typeAliasScope,extendedGenVarEnv,set, mt1.codomain, mt2.codomain,deep+1)
          }
          set
        case (p1: PrimType, p2: PrimType) =>
          if(printRules) println(s"$spaces [Prim]")
          if(p1 == p2) newSet
          else throw SubtypingError(p1,p2)//"Subtyping between primitive types is nominal" // innerSubType(delta,newSet, p1.toObjType, p2.toObjType,deep+1)
        case (p1: PrimType, _) =>
          if(printRules) println(s"$spaces [PrimL]")
          innerSubType(typeAliasScope,deltaInst,newSet, toRecordType(p1), t2,deep+1)
        //case (_, p2: PrimType) =>
        //  if(printRules) println(s"$spaces [PrimR]")
        //  innerSubType(delta,newSet, t1, toRecordType(p2),deep+1)
        case (ot1@ObjectType(_, _), _) =>
          if(printRules) println(s"$spaces [ObjL]")
          innerSubType(typeAliasScope,deltaInst,newSet, unfold(ot1), t2,deep+1)
        case (_, ot2@ObjectType(_, _)) /*if !ot2.isPrimitive */ =>
          if(printRules) println(s"$spaces [ObjR]")
          innerSubType(typeAliasScope,deltaInst,newSet, t1, unfold(ot2),deep+1)
        case (_, ot2:IBuiltinObject) /*if !ot2.isPrimitive */ => //hack to create some builtin object that are not primitive types
          if(printRules) println(s"$spaces [ObjRBuiltin]")
          innerSubType(typeAliasScope,deltaInst,newSet, t1, ot2.toObjType,deep+1)
        case _ => throw SubtypingError(t1,t2)
      }
    }
  }

  private def unfold(t1: ObjectType): LabelE = {

    TypeSubstE.subst(RecordTypeE(t1.methods).setIsPrimitive(false), t1.selfVar, t1,mutable.HashMap[String,String]())
  }
  private def toRecordType(t:PrimType):RecordTypeE = {
    RecordTypeE(t.methods).setIsPrimitive(true)
  }
}
case class SubtypingError(t1:LabelE,t2:LabelE) extends Error {
  var message: String = ""
  def setMessage(s:String):SubtypingError={
    message=s
    this
  }
}
