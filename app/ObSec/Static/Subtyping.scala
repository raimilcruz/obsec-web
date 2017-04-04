package ObSec.Static

import ObSec.Ast._

/**
  * Created by rcc on 4/2/2017.
  */
//TODO: Use implicits for creating infix notation S1 <:: S2
trait SubTypingAlgorithm {

  def <::(t1: Type, t2: Type): Boolean

  def <::(s1: SType, s2: SType): Boolean = <::(s1.privateType, s2.publicType) && <::(s1.privateType, s2.publicType)

  protected def <::(m1: MType, m2: MType): Boolean = {
    m1.domain.size ==  m2.domain.size && m2.domain.zip(m1.domain).forall(pair => <::(pair._1, pair._2)) && <::(m1.codomain, m2.codomain)
  }
}


class AmadioCardelliSubtyping extends SubTypingAlgorithm {

  override def <::(t1: Type, t2: Type): Boolean = innerSubType(Set(), t1, t2)

  private def <::(alreadySeen: Set[Tuple2[Type, Type]], s1: SType, s2: SType) = {
    innerSubType(alreadySeen, s1.publicType, s2.publicType) && innerSubType(alreadySeen, s1.privateType, s2.privateType)
  }

  private def innerSubType(alreadySeen: Set[Tuple2[Type, Type]], t1: Type, t2: Type): Boolean = {
    if (alreadySeen.exists((x)=> TypeEquivalence.alphaEq(x._1,t1) && TypeEquivalence.alphaEq(x._2,t2))) true
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      (t1, t2) match {
        case (_,t) if TypeEquivalence.alphaEq(t, ObjType.top) => true
          //litter optimization
        //case (_,_) if TypeEquivalence.alphaEq(t1,t2) =>true
        case (RecordType(methodsR1), RecordType(methodsR2)) =>
          methodsR2.forall(m2 => {
            val m1 = methodsR1.find(x => x.name == m2.name)
            m1 match {
              case None => false
              case Some(m11) =>
                println(s"m1: ${m11}")
                println(s"m1: ${m2}")

                m2.mtype.domain.zip(m11.mtype.domain).forall(pair=> <::(alreadySeen, pair._1, pair._2)) &&
                  <::(alreadySeen, m11.mtype.codomain, m2.mtype.codomain)
            }
          })
        case (ot1@ObjType(_, _), _) => innerSubType(newSet, unfold(ot1), t2)
        case (_, ot2@ObjType(_, _)) => innerSubType(newSet, t1, unfold(ot2))
        case (p1: PrimType, _) => innerSubType(newSet,p1.toObjType,t2)
        case (_,p2:PrimType) => innerSubType(newSet,t1,p2.toObjType)
        case _ => false
      }
    }
  }

  private def unfold(t1: ObjType): Type = {
    TypeSubst.subst(RecordType(t1.methods),t1.typeVar.name,t1)
  }
}