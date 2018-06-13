package ObSecG.Static

import ObSecG.Ast._

trait ITypeSubsG{
  /**
    * [t2/x]t. Substitutes the recursive variable x by t2 in t.
    * @param t
    * @param x
    * @param t2
    * @return
    */
  def substRecVar(t: LabelG, x: String, t2: LabelG): LabelG
  def substLabel(t: LabelG, labelVar: BoundedLabelVar, t2: LabelG): LabelG
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
  def substRecVar(t: LabelG, x: String, t2: LabelG): LabelG = t match {
    case p:PrimType => p
    case TypeVar(y: String) => if (x == y) t2 else t
    case gv:LabelVar => gv
    case UnionLabel(l1,l2)=>
      UnionLabel(substRecVar(l1,x,t2),substRecVar(l2,x,t2))
    case RecordTypeG(methods) =>
      RecordTypeG(methods.map(m =>
        MethodDeclarationG(m.name,
          MTypeG(
            m.mType.typeVars,
            m.mType.domain.map(stype =>
              STypeG(
                substRecVar(stype.privateType, x, t2).asInstanceOf[TypeG],
                substRecVar(stype.publicType, x, t2))),
            STypeG(
              substRecVar(m.mType.codomain.privateType, x, t2).asInstanceOf[TypeG],
              substRecVar(m.mType.codomain.publicType, x, t2)
          )))))
    case ot@ObjectType(y, methods) =>
      if (y == x) t
      else {
        var newVar = getFreshSelfVarNotIn(x, List(y, x) ++ freeSelfVars(t) ++ freeSelfVars(t2))
        ObjectType(
          newVar,
          methods.map(m =>
            MethodDeclarationG(
              m.name,
              MTypeG(
                m.mType.typeVars.map(st=>
                  BoundedLabelVar(
                    st.typeVar,
                    substRecVar(st.lowerBound,y,TypeVar(newVar)),
                    substRecVar(st.upperBound,y,TypeVar(newVar))
                  ).setAster(st.isAster)
                ),
                m.mType.domain.map(stype => STypeG(
                  substRecVar(substRecVar(stype.privateType, y, TypeVar(newVar)),
                  x, t2).asInstanceOf[TypeG],
                  substRecVar(substRecVar(stype.publicType, y, TypeVar(newVar)),
                  x, t2))),
              STypeG(
                substRecVar(substRecVar(m.mType.codomain.privateType, y, TypeVar(newVar)),
                  x, t2).asInstanceOf[TypeG],
                substRecVar(substRecVar(m.mType.codomain.publicType, y, TypeVar(newVar)),
                  x, t2))
            )))).setIsPrimitive(ot.isPrimitive)
      }
  }
  def substLabel(containerType: LabelG, labelVar: BoundedLabelVar, actualType: LabelG): LabelG = {
    if (labelVar.isAster && TypeEquivalenceG.alphaEq(labelVar.lowerBound,actualType)) {
      auxiliaryFunctions.normalize(containerType,labelVar)
    }
    else {
      substLabelVar(containerType,labelVar.typeVar,actualType)
    }
  }

    //let assume that t2 is closed (it does not contains free type vars)
  private def substLabelVar(t: LabelG, x: String, t2: LabelG): LabelG = t match {
    case p:PrimType => p
    case TypeVar(y: String) => t
    case lv@LabelVar(y) => if(x==y) t2 else t
    case RecordTypeG(methods) =>
      RecordTypeG(methods.map(m =>
        MethodDeclarationG(m.name,
          MTypeG(
            m.mType.typeVars,
            m.mType.domain.map(stype =>
              STypeG(
                substLabelVar(stype.privateType, x, t2).asInstanceOf[TypeG],
                substLabelVar(stype.publicType, x, t2))),
            STypeG(
              substLabelVar(m.mType.codomain.privateType, x, t2).asInstanceOf[TypeG],
              substLabelVar(m.mType.codomain.publicType, x, t2)
            )))))
    case ObjectType(y, methods) =>
      ObjectType(
        y,
        methods.map(m =>
          //do not substitute if there is a variable with that name
          if(m.mType.typeVars.exists(p=> p.typeVar == x)) m
          else
            MethodDeclarationG(
              m.name,
              MTypeG(
                m.mType.typeVars,
                m.mType.domain.map(stype => STypeG(
                  substLabelVar(stype.privateType,x, t2).asInstanceOf[TypeG],
                  substLabelVar(stype.publicType,x, t2))),
                STypeG(
                  substLabelVar(m.mType.codomain.privateType, x, t2).asInstanceOf[TypeG],
                  substLabelVar(m.mType.codomain.publicType, x, t2))
              ))))
    case UnionLabel(left,right)=>
      UnionLabel(substLabelVar(left, x, t2),substLabelVar(right, x, t2))
      //TODO: Implement normarlization of UnionLabel after substituting X*s

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
                           t: LabelG): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    //case GenericTypeVar(x) => List()
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

  private def freeSelfVars(t: LabelG): List[String] = freeSelfVars(Set(), t)
}
object TypeSubstG{
  def substLabel(containerType: LabelG, tv: BoundedLabelVar, actualLabel: LabelG):LabelG = {
    val instance = new TypeSubstG(new AuxiliaryFunctions)
    instance.substLabel(containerType,tv,actualLabel)
  }

  def substRecVar(t: LabelG, x: String, t2: LabelG): LabelG = {
    val instance = new TypeSubstG(new AuxiliaryFunctions)
    instance.substRecVar(t,x,t2)
  }
}