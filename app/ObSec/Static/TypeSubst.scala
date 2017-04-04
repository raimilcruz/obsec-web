package ObSec.Static

import ObSec.Ast._

/**
  * Created by racruz on 04-04-2017.
  */
object TypeSubst {
  def subst(t: Type, x: String, t2: Type): Type = t match {
    case IntType => IntType
    case StringType => StringType
    case BooleanType => BooleanType
    case TypeVar(y: String) => if (x == y) t2 else t
    case RecordType(methods) =>
      RecordType(methods.map(m => MethodDeclaration(m.name,
        MType(
          m.mtype.domain.map(stype =>
            SType(
              subst(stype.privateType, x, t2),
              subst(stype.publicType, x, t2))),
          SType(
            subst(m.mtype.codomain.privateType, x, t2),
            subst(m.mtype.codomain.publicType, x, t2)
          )))))
    case ObjType(y, methods) =>
      if (y.name == x) t
      else {
        var newVar = getFreshVarNotIn(x, List(y.name, x) ++ freeVars(t) ++ freeVars(t2))
        ObjType(TypeVar(newVar), methods.map(m =>
          MethodDeclaration(m.name,
            MType(
              m.mtype.domain.map(stype => SType(
                subst(subst(stype.privateType, y.name, TypeVar(newVar)),
                  x, t2),
                subst(subst(stype.publicType, y.name, TypeVar(newVar)),
                  x, t2))),
              SType(
                subst(subst(m.mtype.codomain.privateType, y.name, TypeVar(newVar)),
                  x, t2),
                subst(subst(m.mtype.codomain.publicType, y.name, TypeVar(newVar)),
                  x, t2))
            ))))
      }
  }

  private def getFreshVarNotIn(x: String, strings: List[String]): String = {
    val vars = strings.filter(y => y.startsWith(x)).toList
    for (i <- 1 to vars.size) {
      if (!vars.contains(s"$x$i")) s"$x$i"
    }
    x + (vars.size + 1)
  }

  private def freeVars(set: Set[String], t: Type): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    case ObjType(tv, methods) =>
      val newSet = set + tv.name
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mtype.domain.foldLeft(List[String]())((acc: List[String], t: SType) => acc ++ freeVars(newSet, t.privateType) ++ freeVars(newSet, t.publicType))
        val l2 = freeVars(newSet, m.mtype.codomain.privateType)
        val l3 = freeVars(newSet, m.mtype.codomain.privateType)
        l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    case RecordType(methods) =>
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mtype.domain.foldLeft(List[String]())((acc: List[String], t: SType) => acc ++ freeVars(set, t.privateType) ++ freeVars(set, t.publicType))
        val l2 = freeVars(set, m.mtype.codomain.privateType)
        val l3 = freeVars(set, m.mtype.codomain.privateType)
        l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    case _ => List()
  }

  private def freeVars(t: Type): List[String] = freeVars(Set(), t)
}
