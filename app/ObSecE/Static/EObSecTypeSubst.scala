/*
package ObSecE.Static

import ObSecE.Ast._

import scala.collection.GenTraversableOnce

trait ITypeSubsG{
  /**
    * [t2/x]t. Substitutes the recursive variable x by t2 in t.
    * @param t
    * @param x
    * @param t2
    * @return
    */
  def substRecVar(t: LabelE, x: String, t2: LabelE): LabelE
  def substLabel(t: LabelE, labelVar: ScopedLabelVar, t2: LabelE): LabelE
}
/**
  * Implements avoiding capture substitution for ObSecG.
  */
class TypeSubstG(auxiliaryFunctions: IAuxiliaryFunctions) extends ITypeSubsG {
  /**
    * Substitutes the type variable [x] by the type [t2] in the type
    * [t].
    * Note that type variables reference
    * @param t
    * @param x
    * @param t2
    * @return
    */
  def substRecVar(t: LabelE, x: String, t2: LabelE): LabelE = t match {
    case p:PrimType => p
    case ImplicitLabel => ImplicitLabel
    case TypeVar(y: String) => if (x == y) t2 else t
    case gv:LabelVar => gv
    /*case UnionLabel(l1,l2)=>
      UnionLabel(substRecVar(l1,x,t2),substRecVar(l2,x,t2))*/
    case record@RecordTypeE(methods) =>
      RecordTypeE(methods.map(m =>
        MethodDeclarationE(m.name,
          MTypeE(            
            m.mType.domain.map(stype =>
              stype.map(facet => substRecVar(facet, x, t2))),
            m.mType.codomain.map(facet => substRecVar(facet, x, t2))
          )))).setIsPrimitive(record.isPrimitive)
    case ot@ObjectType(y, methods) =>
      if (y == x) t
      else {
        var newVar = getFreshSelfVarNotIn(x, List(y, x) ++ freeSelfVars(t) ++ freeSelfVars(t2))
        ObjectType(
          newVar,
          methods.map(m =>
            MethodDeclarationE(
              m.name,
              MTypeE(                
                m.mType.domain.map(stype =>
                  stype.map(facet => substRecVar(substRecVar(facet, y, TypeVar(newVar)),x, t2))),
                m.mType.codomain.map(facet => substRecVar(substRecVar(facet, y, TypeVar(newVar)),x, t2))
            ))))//.setIsPrimitive(ot.isPrimitive)
      }
    case c => throw new NotImplementedError(s"Case $c is not implemented")

  }
  def substLabel(containerType: LabelE, labelVar: ScopedLabelVar, actualType: LabelE): LabelE = {
    ???
  }


  private def getFreshSelfVarNotIn(x: String,
                                   strings: List[String]): String = {
    val vars = strings.filter(y => y.startsWith(x)).toList
    for (i <- 1 to vars.size) {
      if (!vars.contains(s"$x$i")) s"$x$i"
    }
    x + (vars.size + 1)
  }

  private def freeSelfVars(set: Set[String],
                           t: LabelE): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    //case GenericTypeVar(x) => List()
    case ObjectType(tv, methods) =>
      val newSet = set + tv
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeE) =>
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
    case RecordTypeE(methods) =>
      var result = List[String]()
      val res = methods.map(m => {
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeE) =>
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

  private def freeSelfVars(t: LabelE): List[String] = freeSelfVars(Set(), t)

  private def freeLabelVars(t: LabelE):List[String]= freeLabelVars(Set(),t)

  private def freeLabelVars(set: Set[String],
                           t: LabelE): List[String] = t match {
    case LabelVar(x) => if (set.contains(x)) List() else List(x)
    case _ if t.isInstanceOf[RecordTypeE] || t.isInstanceOf[ObjectType]  =>
      val methods =  t match {
        case ObjectType(_,meths) => meths
        case RecordTypeE(meths) => meths
      }
      var result = List[String]()
      var newSet = set
      val res = methods.map(m => {        
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeE) =>
              acc ++ freeLabelVars(newSet, t.privateType) ++
                freeLabelVars(newSet, t.publicType))
        val l2 = freeLabelVars(newSet, m.mType.codomain.privateType)
        val l3 = freeLabelVars(newSet, m.mType.codomain.privateType)
        l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result  
    case _ => List()
  }
}
object TypeSubstG{
  def substLabel(containerType: LabelE, tv: ScopedLabelVar, actualLabel: LabelE):LabelE = {
    val instance = new TypeSubstG(new AuxiliaryFunctions)
    instance.substLabel(containerType,tv,actualLabel)
  }

  def substRecVar(t: LabelE, x: String, t2: LabelE): LabelE = {
    val instance = new TypeSubstG(new AuxiliaryFunctions)
    instance.substRecVar(t,x,t2)
  }

}*/
