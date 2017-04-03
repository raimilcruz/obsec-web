package ObSec.Static

import ObSec.Ast._

/**
  * Created by rcc on 4/2/2017.
  */
//TODO: Use implicits for creating infix notation S1 <:: S2
trait SubTypingAlgorithm {

  //Internal record type (used when a object recursive type is unfolded
  case class RecordType(methods: List[MethodDeclaration]) extends Type {
    override def methSig(x: String): MType = throw new NotImplementedError("Not important")

    override def containsMethod(x: String): Boolean = throw new NotImplementedError("Not important")
  }

  def <::(t1: Type, t2: Type): Boolean

  def <::(s1: SType, s2: SType): Boolean = <::(s1.privateType, s2.publicType) && <::(s1.privateType, s2.publicType)

  protected def <::(m1: MType, m2: MType): Boolean = {
    <::(m2.domain, m1.domain) && <::(m1.codomain, m2.codomain)
  }
  def alphaEq(t1:Type,t2:Type):Boolean = recAlphaEq(Set(),t1,t2)
  def recAlphaEq(set: Set[Tuple2[String, String]], t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (TypeVar(x), TypeVar(y)) => x == y || set.contains(Tuple2(x, y))
    case (ObjType(x, methods1), ObjType(y, methods2)) =>
      val newSet = set + Tuple2(x.name, y.name)
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get
          recAlphaEq(newSet, m.mtype.domain.privateType, method2.mtype.domain.privateType) &&
            recAlphaEq(newSet, m.mtype.domain.publicType, method2.mtype.domain.publicType) &&
            recAlphaEq(newSet, m.mtype.codomain.privateType, method2.mtype.codomain.privateType) &&
            recAlphaEq(newSet, m.mtype.codomain.publicType, method2.mtype.codomain.publicType)
        })
    case (RecordType(methods1), RecordType(methods2)) =>
      if (methods1.map(x => x.name).toSet != methods2.map(x => x.name).toSet)
        false
      else
        methods1.forall(m => {
          val method2 = methods2.find(m2 => m2.name == m.name).get
          recAlphaEq(set, m.mtype.domain.privateType, method2.mtype.domain.privateType) &&
            recAlphaEq(set, m.mtype.domain.publicType, method2.mtype.domain.publicType) &&
            recAlphaEq(set, m.mtype.codomain.privateType, method2.mtype.codomain.privateType) &&
            recAlphaEq(set, m.mtype.codomain.publicType, method2.mtype.codomain.publicType)
        })
    case (_, _) => t1.equals(t2)
  }
}

//TODO: Implement subtyping for recursive types
class AmadioCardelliSubtyping extends SubTypingAlgorithm {


  override def <::(t1: Type, t2: Type): Boolean = innerSubType(Set(), t1, t2)

  private def <::(alreadySeen: Set[Tuple2[Type, Type]], s1: SType, s2: SType) = {
    innerSubType(alreadySeen, s1.publicType, s2.publicType) && innerSubType(alreadySeen, s1.privateType, s2.privateType)
  }


  private def innerSubType(alreadySeen: Set[Tuple2[Type, Type]], t1: Type, t2: Type): Boolean = {
    if (alreadySeen.exists((x)=> alphaEq(x._1,t1) && alphaEq(x._2,t2))) true
    else {
      val newSet = alreadySeen + Tuple2(t1, t2)
      (t1, t2) match {
        case (_,t) if alphaEq(t, ObjType.top) => true
        case (RecordType(methodsR1), RecordType(methodsR2)) =>
          methodsR2.forall(m2 => {
            val m1 = methodsR1.find(x => x.name == m2.name)
            m1 match {
              case None => false
              case Some(m11) =>
                println(s"methods1 $methodsR1")
                println(s"methods2 $methodsR2")
                <::(alreadySeen, m2.mtype.domain, m11.mtype.domain) &&
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
    subst(RecordType(t1.methods),t1.typeVar.name,t1)
  }


  def getFreshVarNotIn(x: String, strings: List[String]): String = {
    val vars = strings.filter(y => y.startsWith(x)).toList
    for (i <- 1 to vars.size) {
      if (!vars.contains(s"$x$i")) s"$x$i"
    }
    x + (vars.size + 1)
  }

  def freeVars(set: Set[String], t: Type): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    case ObjType(tv, methods) =>
      val newSet = set + tv.name
      var result = List[String]()
      val res = methods.map(m =>
        freeVars(newSet, m.mtype.domain.privateType) ++ freeVars(newSet, m.mtype.domain.publicType)
          ++ freeVars(newSet, m.mtype.codomain.privateType)
          ++ freeVars(newSet, m.mtype.codomain.privateType))
      for (l <- res) {
        result = result ++ l
      }
      result
    case RecordType(methods) =>
      var result = List[String]()
      val res = methods.map(m =>
        freeVars(set, m.mtype.domain.privateType) ++ freeVars(set, m.mtype.domain.publicType)
          ++ freeVars(set, m.mtype.codomain.privateType)
          ++ freeVars(set, m.mtype.codomain.privateType))
      for (l <- res) {
        result = result ++ l
      }
      result
    case _ => List()
  }

  def freeVars(t: Type): List[String] = freeVars(Set(), t)

  def subst(t: Type, x: String, t2: Type): Type = t match {
    case IntType => IntType
    case StringType => StringType
    case BooleanType => BooleanType
    case TypeVar(y: String) => if (x == y) t2 else t
    case RecordType(methods) =>
      RecordType(methods.map(m => MethodDeclaration(m.name,
        MType(
          SType(
            subst(m.mtype.domain.privateType, x, t2),
            subst(m.mtype.domain.publicType, x, t2)),
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
              SType(
                subst(subst(m.mtype.domain.privateType, y.name, TypeVar(newVar)),
                  x, t2),
                subst(subst(m.mtype.domain.publicType, y.name, TypeVar(newVar)),
                  x, t2)),
              SType(
                subst(subst(m.mtype.codomain.privateType, y.name, TypeVar(newVar)),
                  x, t2),
                subst(subst(m.mtype.codomain.publicType, y.name, TypeVar(newVar)),
                  x, t2))
            ))))
      }
  }
}