package ObSec.Static

import ObSec.Ast._

/**
  * Created by rcc on 4/2/2017.
  */
//TODO: Use implicits for creating infix notation S1 <:: S2
trait SubTypingAlgorithm {

  def <::(t1: Type, t2: Type): Boolean

  def <::(s1: SType, s2: SType): Boolean = <::(s1.privateType, s2.privateType) && <::(s1.publicType, s2.publicType)

  protected def <::(m1: MType, m2: MType): Boolean = {
    m1.domain.size == m2.domain.size && m2.domain.zip(m1.domain).forall(pair => <::(pair._1, pair._2)) && <::(m1.codomain, m2.codomain)
  }
}


class AmadioCardelliSubtyping extends SubTypingAlgorithm {

  override def <::(t1: Type, t2: Type): Boolean =
    try {
      val res = innerSubType(Set(), t1, t2)
      true
    } catch {
      case x: SubtypingError => false
    }

  private def <::(alreadySeen: Set[Tuple2[Type, Type]], s1: SType, s2: SType): SubtypinAssumptions = {
    val set = innerSubType(alreadySeen, s1.publicType, s2.publicType)
    innerSubType(set, s1.privateType, s2.privateType)
  }

  type SubtypinAssumptions = Set[Tuple2[Type, Type]]

  private def innerSubType(alreadySeen: SubtypinAssumptions, t1: Type, t2: Type): SubtypinAssumptions = {
    println(s"Already seen: ${alreadySeen}")
    println(s"st goal: ${t1} and ${t2}")
    if (alreadySeen.exists((x) => TypeEquivalence.alphaEq(x._1, t1) && TypeEquivalence.alphaEq(x._2, t2)))
      alreadySeen
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      println("In subtyping rules")
      (t1, t2) match {
        case (_, t) if TypeEquivalence.alphaEq(t, ObjType.top) => newSet
        //little optimization
        //case (_,_) if TypeEquivalence.alphaEq(t1,t2) =>true
        case (RecordType(methodsR1), RecordType(methodsR2)) =>
          var set = newSet
          for (m2 <- methodsR2) {
            val m1 = methodsR1.find(x => x.name == m2.name)
            m1 match {
              case None => throw SubtypingError("Method not in object")
              case Some(m11) =>
                for (pair <- m2.mtype.domain.zip(m11.mtype.domain)) {
                  set = <::(set, pair._1, pair._2)
                }
                set = <::(set, m11.mtype.codomain, m2.mtype.codomain)
            }
          }
          set
        case (ot1@ObjType(_, _), _) => innerSubType(newSet, unfold(ot1), t2)
        case (_, ot2@ObjType(_, _)) => innerSubType(newSet, t1, unfold(ot2))
        case (p1: PrimType, p2: PrimType) => if(p1 == p2) newSet else innerSubType(newSet, p1.toObjType, p2.toObjType)
        case (_, p2: PrimType) => innerSubType(newSet, t1, p2.toObjType)
        case (p1: PrimType, _) => innerSubType(newSet, p1.toObjType, t2)
        case _ => throw SubtypingError("Not!")
      }
    }
  }

  private def unfold(t1: ObjType): Type = {
    TypeSubst.subst(RecordType(t1.methods), t1.typeVar.name, t1)
  }
}

case class SubtypingError(message: String) extends Error