package ObSecG.Static

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._

case class RecordTypeG(methods: List[MethodDeclarationG]) extends TypeG {
  override def toString: String =  s"{${methods.map(x => x.toString).fold("")((x: String, y: String) => x + y).toString}}"
  override def methSig(x: String): MTypeG = throw new NotImplementedError("Not important")
  override def containsMethod(x: String): Boolean = throw new NotImplementedError("Not important")
}

abstract class ISubtypingGObSec(
                                 judgements: GObSecJudgements,
                                 errors: ErrorCollector)
  extends IJudgment(
    judgements: GObSecJudgements,
    errors: ErrorCollector){

  def <::(labelVariableEnv:LabelVarEnvironment,t1: LabelG, t2: LabelG): Boolean
  def <::(labelVariableEnv:LabelVarEnvironment, s1:STypeG,s2:STypeG):Boolean
}

class AmadioCardelliSubtypingG(
                                judgements: GObSecJudgements,
                                errors: ErrorCollector)
  extends ISubtypingGObSec(
    judgements: GObSecJudgements,
    errors: ErrorCollector) {

  val auxiliaryFunctions = new AuxiliaryFunctions

  def <::(labelVariableEnv:LabelVarEnvironment,t1: LabelG, t2: LabelG): Boolean =
    try {
      val res = innerSubType(labelVariableEnv,Set(), t1, t2)
      true
    } catch {
      case x: SubtypingError => false
    }

  def <::(labelVariableEnv:LabelVarEnvironment,s1: STypeG, s2: STypeG): Boolean =
    <::(labelVariableEnv,s1.privateType, s2.privateType) &&
      <::(labelVariableEnv,s1.publicType, s2.publicType)

  private def <::(labelVariableEnv:LabelVarEnvironment,
                  alreadySeen: SubtypingAssumptions,
                  s1: STypeG,
                  s2: STypeG): SubtypingAssumptions = {
    val set = innerSubType(labelVariableEnv,alreadySeen, s1.publicType, s2.publicType)
    innerSubType(labelVariableEnv,set, s1.privateType, s2.privateType)
  }

  private def equivalentTypeVariablesAndConstraints(m11: MethodDeclarationG,
                                                    m2: MethodDeclarationG):Boolean = {
    /*if(m11.mType.typeVars.lengthCompare(m2.mType.typeVars.size) != 0)
      false
    m11.mType.typeVars.zip(m2.mType.typeVars).forall(p =>
      p._1.typeVar == p._2.typeVar ||
        TypeEquivalenceG.alphaEq(p._1.typeBound, p._1.typeBound)
    )*/
    throw new Error("Not implemented: equivalentTypeVariablesAndConstraints")
  }

  private def innerSubType(labelVariableEnv: Environment[TypeVarBounds],
                           alreadySeen: SubtypingAssumptions,
                           t1: LabelG,
                           t2: LabelG): SubtypingAssumptions= {
    println("**********************")
    println(s"Generic var constraint : $alreadySeen")
    println(s"Already seen: $alreadySeen")
    println(s"Already seen: $labelVariableEnv")
    println(s"st goal: $t1 and $t2")
    println("*********************")

    if (alreadySeen.exists((x) => TypeEquivalenceG.alphaEq(x._1, t1) &&
      TypeEquivalenceG.alphaEq(x._2, t2)))
      alreadySeen
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      //println("In subtyping rules")
      (t1, t2) match {
        case (_, t) if TypeEquivalenceG.alphaEq(t, ObjectType.top) => newSet
        //little optimization
        //case (_,_) if TypeEquivalence.alphaEq(t1,t2) =>true
        case (gv1: LabelVar,gv2:LabelVar) =>
          //little optimization
          if(gv1 == gv2)
            newSet
          else {
            val bounds1 = labelVariableEnv.lookup(gv1.name)
            val bounds2 = labelVariableEnv.lookup(gv2.name)
            val set = innerSubType(
              labelVariableEnv,
              newSet,
              bounds2.lower,
              bounds1.lower)
            innerSubType(
              labelVariableEnv,
              set,
              bounds1.upper,
              bounds2.upper)
          }
        case (gv1: LabelVar,t) =>
          val upperBound = labelVariableEnv.lookup(gv1.name).upper
          if(upperBound == t)
            newSet
          else
            innerSubType(
              labelVariableEnv,
              newSet,
              upperBound,
              t)
        case (t,gv2:LabelVar) =>
          val lowerBound = labelVariableEnv.lookup(gv2.name).lower
          if(lowerBound == t)
            newSet
          else
            innerSubType(
              labelVariableEnv,
              newSet,
              t,
              lowerBound)
        case (RecordTypeG(methodsR1), RecordTypeG(methodsR2)) =>
          var set = newSet
          for (m2 <- methodsR2) {
            val m1 = methodsR1.find(x => x.name == m2.name)
            m1 match {
              case None => throw SubtypingError("Method not in object")
              case Some(m11) =>
                /*
                Gamma , X:L2..U2 |- T1 <: T2
                ----------------------------------
                Gamma |- [X:L1..U1]. T1 <: [X:L2..U2].T2
                * */
                val newSet:Set[(LabelG,LabelG)] =
                  m11
                    .mType
                    .typeVars
                    .map(c => (TypeVar(c.typeVar),c.upperBound))
                    .toSet[(LabelG,LabelG)]
                set = set.union(newSet)
                val extendedGenVarEnv = auxiliaryFunctions.multiExtend(labelVariableEnv,m11.mType.typeVars)
                for (pair <- m2.mType.domain.zip(m11.mType.domain)) {
                  set = <::(extendedGenVarEnv,set, pair._1, pair._2)
                }
                set = <::(extendedGenVarEnv,set, m11.mType.codomain, m2.mType.codomain)
            }
          }
          set
        case (ot1@ObjectType(_, _), _) =>
          innerSubType(labelVariableEnv,newSet, unfold(ot1), t2)
        case (_, ot2@ObjectType(_, _)) =>
          if(ot2.isPrimitive)
            throw SubtypingError("Not!")
          innerSubType(labelVariableEnv,newSet, t1, unfold(ot2))
        case (p1: PrimType, p2: PrimType) =>
          if(p1 == p2) newSet
          else innerSubType(labelVariableEnv,newSet, p1.toObjType, p2.toObjType)
        case (p1: PrimType, _) =>
          innerSubType(labelVariableEnv,newSet, p1.toObjType, t2)
        case (union@UnionLabel(t11,t12),_)=>
          val set = innerSubType(labelVariableEnv,newSet,t11,t2)
          innerSubType(labelVariableEnv,set,t12,t2)
        case (_,union@UnionLabel(t21,t22))=>
          //it should be one or the other one
          try {
            innerSubType(labelVariableEnv, newSet, t1, t21)
          }
          catch {
            case x: SubtypingError => innerSubType(labelVariableEnv,newSet,t1,t22)
          }
        case _ => throw SubtypingError("Not!")
      }
    }
  }

  private def unfold(t1: ObjectType): LabelG = {
    TypeSubstG.substRecVar(RecordTypeG(t1.methods), t1.selfVar, t1)
  }
}

case class SubtypingError(message: String) extends Error