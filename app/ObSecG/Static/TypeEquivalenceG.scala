package ObSecG.Static

import ObSecG.Ast._

/**
  * Implements type equivalence for ObSecG.
  */
object TypeEquivalenceG {
  /**
    * Alpha equivalence for types
    * @param t1
    * @param t2
    * @return
    */
  def alphaEq(t1:LabelG,t2:LabelG):Boolean = recAlphaEq(Set(),t1,t2)

  /**
    * Alpha equivalence for security types
    * @param s1
    * @param s2
    * @return
    */
  def alphaEq(s1:STypeG,s2:STypeG):Boolean =
    alphaEq(s1.privateType,s2.privateType) && alphaEq(s1.publicType,s2.publicType)


  private def recAlphaEq(set: Set[Tuple2[String, String]],
                         t1: LabelG,
                         t2: LabelG): Boolean = (t1, t2) match {
    case (TypeVar(x), TypeVar(y)) => x == y || set.contains(Tuple2(x, y))
    //case (GenericTypeVar(x),GenericTypeVar(y)) => x==y
    case (ObjectType(x, methods1), ObjectType(y, methods2)) =>
      val newSet = set + Tuple2(x, y)
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get

          //equivalent type parameter
          m.mType.typeVars.zip(method2.mType.typeVars).forall(pair =>
            recAlphaEq(newSet,pair._1.lowerBound,pair._2.lowerBound) &&
              recAlphaEq(newSet,pair._1.upperBound,pair._2.upperBound)
          ) &&
          //equivalent arguments
          m.mType.domain.zip(method2.mType.domain).forall(pair =>
            recAlphaEq(
              newSet,
              pair._1.privateType,
              pair._2.privateType) &&
            recAlphaEq(
              newSet,
              pair._1.publicType,
              pair._2.publicType)) &&
          //equivalent return
          recAlphaEq(newSet,
            m.mType.codomain.privateType,
            method2.mType.codomain.privateType) &&
          recAlphaEq(newSet,
            m.mType.codomain.publicType,
            method2.mType.codomain.publicType)
        })
    case (RecordTypeG(methods1), RecordTypeG(methods2)) =>
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get
          m.mType.domain.zip(method2.mType.domain).forall(pair =>
            recAlphaEq(set,pair._1.privateType,pair._2.privateType) &&
              recAlphaEq(set,pair._1.publicType,pair._2.publicType)) &&
            recAlphaEq(set, m.mType.codomain.privateType, method2.mType.codomain.privateType) &&
            recAlphaEq(set, m.mType.codomain.publicType, method2.mType.codomain.publicType)
        })
    /*case (UnionLabel(l1,r1),UnionLabel(l2,r2)) =>
      recAlphaEq(set,l1,l2) && recAlphaEq(set,r1,r2)*/
    case (_, _) => t1.equals(t2)
  }
}

