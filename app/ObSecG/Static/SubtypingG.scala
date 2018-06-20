package ObSecG.Static

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._

case class RecordTypeG(methods: List[MethodDeclarationG]) extends TypeG with Primitable{
  override def toString: String =  s"{${methods.map(x => x.toString).fold("")((x: String, y: String) => x + y).toString}}"
  override def methSig(x: String): MTypeG = throw new NotImplementedError("Not important")
  override def containsMethod(x: String): Boolean = throw new NotImplementedError("Not important")

  override def prettyPrint(builder: StringBuilder): Unit = {
    builder.append("[")
    methods.map(e=> {
      builder.append("\n")
      e.prettyPrint(builder)
    })
    builder.append("]")
  }
}

sealed trait SubtypingResult {
  def &&(function: => SubtypingResult) : SubtypingResult
}
object SubtypingSuccess extends SubtypingResult {
  override def &&(function: => SubtypingResult): SubtypingResult = function
}
case class SubtypingFail(left: LabelG, righ:LabelG) extends SubtypingResult {
  override def &&(function: => SubtypingResult): SubtypingResult = this

  var message:String = ""
  def setMessage(s:String):SubtypingFail={
    message = s
    this
  }
}


abstract class ISubtypingGObSec(
                                 judgements: GObSecJudgements,
                                 errors: ErrorCollector)
  extends IJudgment(
    judgements: GObSecJudgements,
    errors: ErrorCollector){

  def <::(labelVariableEnv:LabelVarEnvironment,t1: LabelG, t2: LabelG): SubtypingResult
  def <::(labelVariableEnv:LabelVarEnvironment, s1:STypeG,s2:STypeG):SubtypingResult
}


class AmadioCardelliSubtypingG(
                                judgements: GObSecJudgements,
                                errors: ErrorCollector)
  extends ISubtypingGObSec(
    judgements: GObSecJudgements,
    errors: ErrorCollector) {

  val auxiliaryFunctions = new AuxiliaryFunctions

  def <::(labelVariableEnv:LabelVarEnvironment,t1: LabelG, t2: LabelG): SubtypingResult =
    try {
      val subtypingAssumptions = labelVariableEnv.toList.foldLeft(Set[(LabelG, LabelG)]())(
        (prevSet,tv)=> {
          prevSet + Tuple2(LabelVar(tv._1),tv._2.upper) + Tuple2(tv._2.lower,LabelVar(tv._1))
      })
      val res = innerSubType(labelVariableEnv,subtypingAssumptions, t1, t2,0)
      SubtypingSuccess
    } catch {
      case x: SubtypingError => SubtypingFail(x.t1,x.t2).setMessage(x.message)
    }

  def <::(labelVariableEnv:LabelVarEnvironment,s1: STypeG, s2: STypeG): SubtypingResult =
    <::(labelVariableEnv,s1.privateType, s2.privateType) &&
      <::(labelVariableEnv,s1.publicType, s2.publicType)

  private def <::(labelVariableEnv:LabelVarEnvironment,
                  alreadySeen: SubtypingAssumptions,
                  s1: STypeG,
                  s2: STypeG,
                  deep:Int): SubtypingAssumptions = {
    val set = innerSubType(labelVariableEnv,alreadySeen, s1.publicType, s2.publicType,deep)
    innerSubType(labelVariableEnv,set, s1.privateType, s2.privateType,deep)
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
                           t2: LabelG,
                           deep:Int): SubtypingAssumptions= {

    val printRules = true
    val spaces = (1 to deep).foldLeft("")((acc,x)=>acc+" ")
    println(spaces + " **********************")
    println(spaces + deep)
    println(s"$spaces label env: ${labelVariableEnv.prettyPrint}")
    println(s"$spaces Already seen: ${alreadySeen.mkString(";")}")

    println(s"$spaces st goal: ${t1.prettyPrint()} and ${t2.prettyPrint()}")
    println(spaces + " *********************")

    if (alreadySeen.exists((x) => TypeEquivalenceG.alphaEq(x._1, t1) &&
      TypeEquivalenceG.alphaEq(x._2, t2)))
      alreadySeen
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      //println("In subtyping rules")
      (t1, t2) match {
        // T <: Top
        case (_, t) if TypeEquivalenceG.alphaEq(t, ObjectType.top) =>
          if(printRules) println(s"$spaces [TypeEq]")
          newSet
        //little optimization
        case (_,_) if TypeEquivalenceG.alphaEq(t1,t2) =>newSet
        case (union@UnionLabel(t11,t12),_)=>
          if(printRules) println(s"$spaces [UnionL]")

          val set = innerSubType(labelVariableEnv,newSet,t11,t2,deep+1)
          innerSubType(labelVariableEnv,set,t12,t2,deep+1)
        case (_,union@UnionLabel(t21,t22))=>
          if(printRules) println(s"$spaces [UnionR]")

          //it should be one or the other one
          try {
            innerSubType(labelVariableEnv, newSet, t1, t21,deep+1)
          }
          catch {
            case x: SubtypingError => innerSubType(labelVariableEnv,newSet,t1,t22,deep+1)
          }
        case (gl1:LabelVar,gl2:LabelVar) =>
          if(printRules) println(s"$spaces [Label]")
          //little optimization
          if(gl1 == gl1)
            newSet
          else{
            val bounds1 = labelVariableEnv.lookup(gl1.name)
            val bounds2 = labelVariableEnv.lookup(gl2.name)
            val set = innerSubType(
              labelVariableEnv,
              newSet,
              bounds2.lower,
              bounds1.lower, deep + 1)
            innerSubType(
              labelVariableEnv,
              set,
              bounds1.upper,
              bounds2.upper, deep + 1)
          }
        case (gl1: LabelVar, t) =>
          if(printRules) println(s"$spaces [LabelL]")
          val upperBound = labelVariableEnv.lookup(gl1.name).upper
          if (upperBound == t)
            newSet
          else
            innerSubType(
              labelVariableEnv,
              newSet,
              upperBound,
              t, deep + 1)
        case (t, gv2: LabelVar) =>
          if(printRules) println(s"$spaces [LabelR]")

          val lowerBound = labelVariableEnv.lookup(gv2.name).lower
          if (lowerBound == t)
            newSet
          else
            innerSubType(
              labelVariableEnv,
              newSet,
              t,
              lowerBound, deep + 1)
        case (RecordTypeG(methodsR1), RecordTypeG(methodsR2)) =>
          if(printRules) println(s"$spaces [Record]")

          var set = newSet
          for (m2 <- methodsR2) {
            val mt2 = m2.mType
            val m1Option = methodsR1.find(x => x.name == m2.name)
            if(m1Option.isEmpty)
              throw SubtypingError(t1,t2).setMessage(s"Method ${m2.name} of " +
                s"second type is not in the first type")
            val mt1 = m1Option.get.mType
            /*
            1) Gamma |- U2 <: U1
            2)Gamma |- L1 <: L2
            3) Gamma , X:L2..U2 |- T1 <: T2
            ----------------------------------
            Gamma |- [X:L1..U1]. T1 <: [X:L2..U2].T2
            * */

            //check 1:
            if(mt1.typeVars.size != mt2.typeVars.size)
              throw SubtypingError(t1,t2).setMessage("Type variable size must be the same")

            var mt2Renamed = TypeSubstG.renameLabels(mt2,mt1.typeVars.map(x=>x.typeVar))
            val zippedTypeVars = mt1.typeVars.zip(mt2Renamed.typeVars)

            var extendedGenVarEnv = labelVariableEnv
            for(variablePair  <- zippedTypeVars){
              //condition 1: Gamma |- L1 <: L2
              set = innerSubType(extendedGenVarEnv,set,
                variablePair._1.lowerBound,
                variablePair._2.lowerBound,deep+1)
              //condition 2: Gamma |- U2 <: U1
              set = innerSubType(extendedGenVarEnv,set,variablePair._2.upperBound,variablePair._1.upperBound,deep+1)
              //add constraint to environment
              //Gamma , X:L2..U2
              extendedGenVarEnv = extendedGenVarEnv.extend(variablePair._1.typeVar,variablePair._2.bounds)
            }
            //move type constraint of method 2 to the subtyping assumptions
            val newSet:Set[(LabelG,LabelG)] = mt2Renamed.typeVars.foldLeft(Set[(LabelG, LabelG)]())(
              (prevSet,tv)=> {
                prevSet + Tuple2(LabelVar(tv.typeVar),tv.upperBound) + Tuple2(tv.lowerBound,LabelVar(tv.typeVar))
              })
            set = set.union(newSet)
            for (pair <- mt2Renamed.domain.zip(mt1.domain)) {
              set = <::(extendedGenVarEnv,set, pair._1, pair._2,deep+1)
            }
            set = <::(extendedGenVarEnv,set, mt1.codomain, mt2Renamed.codomain,deep+1)
          }
          set
        case (p1: PrimType, p2: PrimType) =>
          if(printRules) println(s"$spaces [Prim]")
          if(p1 == p2) newSet
          else innerSubType(labelVariableEnv,newSet, p1.toObjType, p2.toObjType,deep+1)
        case (p1: PrimType, _) =>
          if(printRules) println(s"$spaces [PrimL]")
          innerSubType(labelVariableEnv,newSet, p1.toObjType, t2,deep+1)
        case (_, p2: PrimType) =>
          if(printRules) println(s"$spaces [PrimR]")
          innerSubType(labelVariableEnv,newSet, t1, p2.toObjType ,deep+1)
        case (ot1@ObjectType(_, _), _) =>
          if(printRules) println(s"$spaces [ObjL]")
          innerSubType(labelVariableEnv,newSet, unfold(ot1), t2,deep+1)
        case (_, ot2@ObjectType(_, _)) if !ot2.isPrimitive  =>
          if(printRules) println(s"$spaces [ObjR]")
          innerSubType(labelVariableEnv,newSet, t1, unfold(ot2),deep+1)
        case (_, ot2@ObjectType(_, _)) if ot2.isPrimitive  =>
          if(printRules) println(s"$spaces [ObjRPrim]")
          //both type should be equivalent, but not only with alphaEq.
          //A way to verify that is to ask for t1<:ot2 and ot2<:t1
          t1 match {
            case recordOrObject: Primitable if recordOrObject.isPrimitive =>
              innerSubType(labelVariableEnv, newSet, t1, unfold(ot2), deep + 1)
            case _ =>
              val firstSet = innerSubType(labelVariableEnv, newSet, t1, unfold(ot2), deep + 1)
              innerSubType(labelVariableEnv, firstSet, unfold(ot2), t1, deep + 1)
          }

        case _ => throw SubtypingError(t1,t2)
      }
    }
  }

  private def unfold(t1: ObjectType): LabelG = {
    TypeSubstG.substRecVar(RecordTypeG(t1.methods).setIsPrimitive(t1.isPrimitive), t1.selfVar, t1)
  }
}
case class SubtypingError(t1:LabelG,t2:LabelG) extends Error {
  var message: String = ""
  def setMessage(s:String):SubtypingError={
    message=s
    this
  }
}
