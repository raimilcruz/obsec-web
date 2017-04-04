package ObSec.Static

import ObSec.Ast.{ObjType, SType, Type, TypeVar}

/**
  * Created by racruz on 04-04-2017.
  */
object TypeEquivalence {
  /**
    * Alpha equivalence for types
    * @param t1
    * @param t2
    * @return
    */
  def alphaEq(t1:Type,t2:Type):Boolean = recAlphaEq(Set(),t1,t2)

  /**
    * Alpha equivalence for security types
    * @param s1
    * @param s2
    * @return
    */
  def alphaEq(s1:SType,s2:SType):Boolean = alphaEq(s1.privateType,s2.privateType) && alphaEq(s1.publicType,s2.publicType)


  private def recAlphaEq(set: Set[Tuple2[String, String]], t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (TypeVar(x), TypeVar(y)) => x == y || set.contains(Tuple2(x, y))
    case (ObjType(x, methods1), ObjType(y, methods2)) =>
      val newSet = set + Tuple2(x.name, y.name)
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get
          m.mtype.domain.zip(method2.mtype.domain).forall(pair =>
            recAlphaEq(newSet,pair._1.privateType,pair._2.privateType) &&
              recAlphaEq(newSet,pair._1.publicType,pair._2.publicType)) &&
            recAlphaEq(newSet, m.mtype.codomain.privateType, method2.mtype.codomain.privateType) &&
            recAlphaEq(newSet, m.mtype.codomain.publicType, method2.mtype.codomain.publicType)
        })
    case (RecordType(methods1), RecordType(methods2)) =>
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get
          m.mtype.domain.zip(method2.mtype.domain).forall(pair =>
            recAlphaEq(set,pair._1.privateType,pair._2.privateType) &&
              recAlphaEq(set,pair._1.publicType,pair._2.publicType)) &&
            recAlphaEq(set, m.mtype.codomain.privateType, method2.mtype.codomain.privateType) &&
            recAlphaEq(set, m.mtype.codomain.publicType, method2.mtype.codomain.publicType)
        })
    case (_, _) => t1.equals(t2)
  }
}

