package ObSecG.Static

import ObSecG.Ast._

case class RecordTypeG(methods: List[MethodDeclarationG]) extends TypeG {
  override def toString: String =  s"{${methods.map(x => x.toString).fold("")((x: String, y: String) => x + y).toString}}"
  override def methSig(x: String): MTypeG = throw new NotImplementedError("Not important")
  override def containsMethod(x: String): Boolean = throw new NotImplementedError("Not important")
}


class AmadioCardelliSubtypingG {

  def <::(t1: TypeG, t2: TypeG): Boolean =
    try {
      val res = innerSubType(Set(), t1, t2)
      true
    } catch {
      case x: SubtypingError => false
    }

  private def <::(alreadySeen: Set[Tuple2[TypeG, TypeG]], s1: STypeG, s2: STypeG): SubtypinAssumptions = {
    val set = innerSubType(alreadySeen, s1.publicType, s2.publicType)
    innerSubType(set, s1.privateType, s2.privateType)
  }

  type SubtypinAssumptions = Set[Tuple2[TypeG, TypeG]]

  private def innerSubType(alreadySeen: SubtypinAssumptions, t1: TypeG, t2: TypeG): SubtypinAssumptions = {
    // println(s"Already seen: ${alreadySeen}")
    //println(s"st goal: ${t1} and ${t2}")
    if (alreadySeen.exists((x) => TypeEquivalenceG.alphaEq(x._1, t1) && TypeEquivalenceG.alphaEq(x._2, t2)))
      alreadySeen
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      //println("In subtyping rules")
      (t1, t2) match {
        case (_, t) if TypeEquivalenceG.alphaEq(t, ObjectType.top) => newSet
        //little optimization
        //case (_,_) if TypeEquivalence.alphaEq(t1,t2) =>true
        case (RecordTypeG(methodsR1), RecordTypeG(methodsR2)) =>
          var set = newSet
          for (m2 <- methodsR2) {
            val m1 = methodsR1.find(x => x.name == m2.name)
            m1 match {
              case None => throw SubtypingError("Method not in object")
              case Some(m11) =>
                /*
                Gamma , X<:U1 |- S2 <: T2
                Gamma |- forall X<:U1.S2 <: forall X<:U1.T2
                * */
                if(m11.mType.typeVar != m2.mType.typeVar || !TypeEquivalenceG.alphaEq(m11.mType.typeVarBound, m11.mType.typeVarBound))
                  throw SubtypingError("Type variables and type variable bound must be the same! (The first restriction for simplicity)")
                set = set + Tuple2(TypeVar(m11.mType.typeVar),m11.mType.typeVarBound)
                for (pair <- m2.mType.domain.zip(m11.mType.domain)) {
                  set = <::(set, pair._1, pair._2)
                }
                set = <::(set, m11.mType.codomain, m2.mType.codomain)
            }
          }
          set
        case (ot1@ObjectType(_, _), _) => innerSubType(newSet, unfold(ot1), t2)
        case (_, ot2@ObjectType(_, _)) => innerSubType(newSet, t1, unfold(ot2))
        case (p1: PrimType, p2: PrimType) => if(p1 == p2) newSet else innerSubType(newSet, p1.toObjType, p2.toObjType)
        case (_, p2: PrimType) => innerSubType(newSet, t1, p2.toObjType)
        case (p1: PrimType, _) => innerSubType(newSet, p1.toObjType, t2)
        case _ => throw SubtypingError("Not!")
      }
    }
  }

  private def unfold(t1: ObjectType): TypeG = {
    TypeSubstG.substSelf(RecordTypeG(t1.methods), t1.selfVar, t1)
  }
}

case class SubtypingError(message: String) extends Error