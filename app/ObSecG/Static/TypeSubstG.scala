package ObSecG.Static

import ObSecG.Ast._

/**
  * Implements avoiding capture substitution for ObSecG.
  */
object TypeSubstG {
  /**
    * Substitutes the type variable [x] by the type [t2] in the type
    * [t].
    * Note that type variables reference
    * @param t
    * @param x
    * @param t2
    * @return
    */
  def substTypeVar(t: TypeG, x: String, t2: TypeG): TypeG = t match {
    case p:PrimType => p
    case TypeVar(y: String) => if (x == y) t2 else t
    case gv:GenericTypeVar => gv
    case RecordTypeG(methods) =>
      RecordTypeG(methods.map(m =>
        MethodDeclarationG(m.name,
          MTypeG(
            m.mType.typeVars,
            m.mType.domain.map(stype =>
              STypeG(
                substTypeVar(stype.privateType, x, t2),
                substTypeVar(stype.publicType, x, t2))),
            STypeG(
              substTypeVar(m.mType.codomain.privateType, x, t2),
              substTypeVar(m.mType.codomain.publicType, x, t2)
          )))))
    case ObjectType(y, methods) =>
      if (y == x) t
      else {
        var newVar = getFreshSelfVarNotIn(x, List(y, x) ++ freeSelfVars(t) ++ freeSelfVars(t2))
        ObjectType(
          newVar,
          methods.map(m =>
            MethodDeclarationG(
              m.name,
              MTypeG(
                m.mType.typeVars,
                m.mType.domain.map(stype => STypeG(
                  substTypeVar(substTypeVar(stype.privateType, y, TypeVar(newVar)),
                  x, t2),
                  substTypeVar(substTypeVar(stype.publicType, y, TypeVar(newVar)),
                  x, t2))),
              STypeG(
                substTypeVar(substTypeVar(m.mType.codomain.privateType, y, TypeVar(newVar)),
                  x, t2),
                substTypeVar(substTypeVar(m.mType.codomain.publicType, y, TypeVar(newVar)),
                  x, t2))
            ))))
      }
  }


  //def subst(t: TypeG, x: String, t2: TypeG): TypeG = throw  new Error("Not implemented")


  private def getFreshSelfVarNotIn(x: String,
                                   strings: List[String]): String = {
    val vars = strings.filter(y => y.startsWith(x)).toList
    for (i <- 1 to vars.size) {
      if (!vars.contains(s"$x$i")) s"$x$i"
    }
    x + (vars.size + 1)
  }

  private def freeSelfVars(set: Set[String],
                           t: TypeG): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    case GenericTypeVar(x) => List()
    case ObjectType(tv, methods) =>
      val newSet = set + tv
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeG) =>
              acc ++ freeSelfVars(newSet, t.privateType) ++
                freeSelfVars(newSet, t.publicType))
        val l2 = freeSelfVars(newSet, m.mType.codomain.privateType)
        val l3 = freeSelfVars(newSet, m.mType.codomain.privateType)
        l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    case RecordTypeG(methods) =>
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeG) =>
              acc ++ freeSelfVars(set, t.privateType) ++
                freeSelfVars(set, t.publicType))
        val l2 = freeSelfVars(set, m.mType.codomain.privateType)
        val l3 = freeSelfVars(set, m.mType.codomain.privateType)
        l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    case _ => List()
  }

  private def freeSelfVars(t: TypeG): List[String] = freeSelfVars(Set(), t)
}
