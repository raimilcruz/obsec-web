package ObSecG.Static

import ObSecG.Ast._

import scala.collection.GenTraversableOnce

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
    case ImplicitLabel => ImplicitLabel
    case TypeVar(y: String) => if (x == y) t2 else t
    case gv:LabelVar => gv
    /*case UnionLabel(l1,l2)=>
      UnionLabel(substRecVar(l1,x,t2),substRecVar(l2,x,t2))*/
    case record@RecordTypeG(methods) =>
      RecordTypeG(methods.map(m =>
        MethodDeclarationG(m.name,
          MTypeG(
            m.mType.typeVars.map(tv =>
              tv.map(bound => substRecVar(bound, x, t2))),
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
                m.mType.domain.map(stype =>
                  stype.map(facet => substRecVar(substRecVar(facet, y, TypeVar(newVar)),x, t2))),
                m.mType.codomain.map(facet => substRecVar(substRecVar(facet, y, TypeVar(newVar)),x, t2))
            ))))//.setIsPrimitive(ot.isPrimitive)
      }
    case StringGListType(elemPolicy)=> StringGListType(substRecVar(elemPolicy,x,t2))

  }
  def substLabel(containerType: LabelG, labelVar: BoundedLabelVar, actualType: LabelG): LabelG = {
    if (labelVar.isAster && TypeEquivalenceG.alphaEq(labelVar.lowerBound,actualType)) {
      auxiliaryFunctions.normalize(containerType,labelVar)
    }
    else {
      substLabelVar(containerType,labelVar.typeVar,actualType)
    }
  }

  def renameLabels(methType : MTypeG, labelVariables : List[String]): MTypeG= {
    val renameFor = methType.typeVars.map(x=>x.typeVar).zip(labelVariables)

    val newLabelVars = methType.typeVars.zip(labelVariables).map(pair =>
      //rename type variable name and apply substitution for bounds
      pair._1.rename(pair._2).map(bound => substLabelVar(bound, pair._1.typeVar, LabelVar(pair._2)))
    )

    var domain = methType.domain
    var codomain = methType.codomain
    for(pair <- renameFor){
      domain = domain.map(stype =>
        stype.map(facet => substLabelVar(facet, pair._1, LabelVar(pair._2))))

      codomain =  codomain.map(facet => substLabelVar(facet, pair._1, LabelVar(pair._2)))
    }
    MTypeG(newLabelVars,domain,codomain)
  }



  //let assume that t2 is closed (it does not contains free type vars)
  private def substLabelVar(t: LabelG, x: String, t2: LabelG): LabelG = t match {
    case p:PrimType => p
    case ImplicitLabel => ImplicitLabel
    case TypeVar(y: String) => t
    case lv@LabelVar(y) => if(x==y) t2 else t
    case RecordTypeG(methods) =>
      RecordTypeG(methods.map(m => {
        if(m.mType.typeVars.map(x=>x.typeVar).contains(x))
          m
        else {
          var newVar = getFreshSelfVarNotIn(x, List(x) ++ freeLabelVars(t) ++ freeLabelVars(t2))
          MethodDeclarationG(m.name,
            MTypeG(
              m.mType.typeVars,
              m.mType.domain.map(stype =>
                stype.map(facet => substLabelVar(facet, x, t2))),
              m.mType.codomain.map(facet => substLabelVar(facet, x, t2)))
          )
        }
      }))
    case ObjectType(y, methods) =>
      ObjectType(
        y,
        methods.map(m =>
          //do not substitute if there is a variable with that name
          if(m.mType.typeVars.exists(p=> p.typeVar == x)) m
          else{
            var newVar = getFreshSelfVarNotIn(x, List(y, x) ++ freeLabelVars(t) ++ freeLabelVars(t2))
            MethodDeclarationG(
              m.name,
              MTypeG(
                m.mType.typeVars,
                m.mType.domain.map(stype =>
                  stype.map(facet => substLabelVar(facet, x, t2))),
                m.mType.codomain.map(facet => substLabelVar(facet, x, t2))
              ))
          }))
    /*case UnionLabel(left,right)=>
      UnionLabel(substLabelVar(left, x, t2),substLabelVar(right, x, t2))*/
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
        val l0 = m.mType
          .typeVars
          .foldLeft(List[String]())(
            (acc, tv) =>
              acc ++ freeSelfVars(newSet, tv.upperBound) ++
                freeSelfVars(newSet, tv.lowerBound))
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeG) =>
              acc ++ freeSelfVars(newSet, t.privateType) ++
                freeSelfVars(newSet, t.publicType))
        val l2 = freeSelfVars(newSet, m.mType.codomain.privateType)
        val l3 = freeSelfVars(newSet, m.mType.codomain.privateType)
        l0 ++ l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    case RecordTypeG(methods) =>
      var result = List[String]()
      val res = methods.map(m => {
        val l0 = m.mType
          .typeVars
          .foldLeft(List[String]())(
            (acc, tv) =>
              acc ++ freeSelfVars(set, tv.upperBound) ++
                freeSelfVars(set, tv.lowerBound))
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeG) =>
              acc ++ freeSelfVars(set, t.privateType) ++
                freeSelfVars(set, t.publicType))
        val l2 = freeSelfVars(set, m.mType.codomain.privateType)
        val l3 = freeSelfVars(set, m.mType.codomain.privateType)
        l0 ++ l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    /*case UnionLabel(left,right) => freeSelfVars(left) ++ freeSelfVars(right)*/
    case _ => List()
  }

  private def freeSelfVars(t: LabelG): List[String] = freeSelfVars(Set(), t)

  private def freeLabelVars(t: LabelG):List[String]= freeLabelVars(Set(),t)

  private def freeLabelVars(set: Set[String],
                           t: LabelG): List[String] = t match {
    case LabelVar(x) => if (set.contains(x)) List() else List(x)
    case _ if t.isInstanceOf[RecordTypeG] || t.isInstanceOf[ObjectType]  =>
      val methods =  t match {
        case ObjectType(_,meths) => meths
        case RecordTypeG(meths) => meths
      }
      var result = List[String]()
      var newSet = set
      val res = methods.map(m => {
        val l0 = m.mType
          .typeVars
          .foldLeft(List[String]())(
            (acc, tv) => {
              val innerRes = acc ++ freeLabelVars(newSet, tv.upperBound) ++
                freeLabelVars(newSet, tv.lowerBound)
              newSet + tv.typeVar
              innerRes
            })
        val l1 = m.mType
          .domain
          .foldLeft(List[String]())(
            (acc: List[String], t: STypeG) =>
              acc ++ freeLabelVars(newSet, t.privateType) ++
                freeLabelVars(newSet, t.publicType))
        val l2 = freeLabelVars(newSet, m.mType.codomain.privateType)
        val l3 = freeLabelVars(newSet, m.mType.codomain.privateType)
        l0 ++ l1 ++ l2 ++ l3
      })
      for (l <- res) {
        result = result ++ l
      }
      result
    /*case UnionLabel(left,right) => freeLabelVars(left) ++ freeLabelVars(right)*/
    case _ => List()
  }
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
  def renameLabels(mType:MTypeG,labels:List[String]):MTypeG= {
    val instance = new TypeSubstG(new AuxiliaryFunctions)
    instance.renameLabels(mType,labels)
  }

}