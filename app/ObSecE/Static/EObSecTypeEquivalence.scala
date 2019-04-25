package ObSecE.Static

import ObSecE.Ast._

/**
  * Implements type equivalence for ObSecG.
  */
object EObSecTypeEquivalence {

  def alphaEq(t1:LabelE,t2:LabelE):Boolean = recAlphaEq(Set(),t1,t2)

  def alphaEq(s1:STypeE,s2:STypeE):Boolean =
    alphaEq(s1.privateType,s2.privateType) && alphaEq(s1.publicType,s2.publicType)


  private def recAlphaEq(set: Set[(String, String)],
                         t1: LabelE,
                         t2: LabelE): Boolean = (t1, t2) match {
    case (TypeVar(x), TypeVar(y)) => x == y || set.contains(Tuple2(x, y))
    case (ObjectType(x, methods1), ObjectType(y, methods2)) =>
      val newSet = set + Tuple2(x, y)
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get

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
    case (RecordTypeE(methods1), RecordTypeE(methods2)) =>
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
    case (_, _) => t1.equals(t2)
  }
}

