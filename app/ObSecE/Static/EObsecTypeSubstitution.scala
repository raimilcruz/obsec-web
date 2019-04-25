package ObSecE.Static

import java.util

import ObSecE.Ast._

import scala.collection.mutable

trait IEObSecTypeSubst{

  def subst(t: LabelE, x: String, t2: LabelE,labelSubtitution:mutable.HashMap[String,String]): LabelE
}
/**
  * Implements avoiding capture substitution for EObSec.
  */
class TypeSubstE(auxiliaryFunctions: IAuxiliaryFunctions) extends IEObSecTypeSubst {

  def subst(t: LabelE, x: String, t2: LabelE,labelSubtitution:mutable.HashMap[String,String]): LabelE = t match {
    case p:PrimType => p
    case TypeVar(y) => if (x == y) t2 else t
    case LabelVar(y) =>  if (x == y) t2 else t
    case TypeId(y) => if (x == y) t2 else t
    case record:RecordTypeE =>
      RecordTypeE(subst(record.methods,x,t2,labelSubtitution))
    case ot@ObjectType(y, methods) =>
      if (y == x) t
      else {
        val newVar = getFreshVarNotIn(y, List(y, x) ++ freeVars(t) ++ freeVars(t2))
        ObjectType(
          newVar,
          subst(subst(methods,y,TypeVar(newVar),labelSubtitution),x,t2,labelSubtitution))
      }
    case et@ExistentialType(typeVars,methods) => {
      if (typeVars.exists(y => y.name == x))
        t
      else {
        val newExistentialVars =
            typeVars.map(tv =>
              EVarDecl(
                 getFreshVarNotIn(tv.name, List(tv.name,x) ++ freeVars(tv.lowerBound) ++ freeVars(t) ++ freeVars(t2))
                 ,tv.lowerBound))
        typeVars.zip(newExistentialVars).foreach(p =>
          labelSubtitution += (p._1.name -> p._2.name)
        )
        ExistentialType(
          newExistentialVars,
            typeVars.zip(newExistentialVars).map(x=> (x._1.name,x._2.name))
              .foldLeft[List[MethodDeclarationE]](methods)
                ((acc,ev) =>
                  subst(subst(methods,ev._1,LabelVar(ev._2),labelSubtitution),x,t2,labelSubtitution)
              ))
      }
    }
  }
  private def subst(methods: List[MethodDeclarationE], x: String, t2: LabelE,
                    labelSubtitution:mutable.HashMap[String,String]): List[MethodDeclarationE] = {
    methods.map(m =>
      MethodDeclarationE(m.name,
        MTypeE(
          m.mType.domain.map(stype => subst(stype,x,t2,labelSubtitution)),
          subst(m.mType.codomain, x, t2,labelSubtitution)
        )))
  }

  def subst(s: STypeE, x: String, t2: LabelE,labelSubtitution:mutable.HashMap[String,String]): STypeE =
    s.map(f => subst(f,x,t2,labelSubtitution))


  private def getFreshVarNotIn(x: String,
                                   strings: List[String]): String = {
    val vars = strings.filter(y => y.startsWith(x)).toList
    for (i <- 1 to vars.size) {
      if (!vars.contains(s"$x$i")){
        return s"$x$i"
      }
    }
    x + (vars.size + 1)
  }

  private def freeVars(set: Set[String], t: LabelE): List[String] = t match {
    case TypeVar(x) => if (set.contains(x)) List() else List(x)
    case ObjectType(tv, methods) =>
      val newSet = set + tv
      freeVars(newSet,methods)
    case RecordTypeE(methods) =>
      freeVars(set,methods)
    case ExistentialType(existentialVars,methods)=>
      val newSet = set ++ existentialVars.map(x=>x.name).toSet
      freeVars(newSet,methods)
    case _ => List()
  }
  private def freeVars(set: Set[String], methods: List[MethodDeclarationE]): List[String] ={
    var result = List[String]()
    val res = methods.map(m => {
      val l1 = m.mType
        .domain
        .foldLeft(List[String]())(
          (acc: List[String], t: STypeE) =>
            acc ++ freeVars(set, t))
      val l2 = freeVars(set, m.mType.codomain)
      l1 ++ l2
    })
    for (l <- res) {
      result = result ++ l
    }
    result
  }


  private def freeVars(set: Set[String], t: STypeE): List[String] =
    t.children.foldLeft(List[String]())((acc,x) => acc ++ freeVars(set,x))


  private def freeVars(t: LabelE): List[String] = freeVars(Set[String](), t)


  def renameLabels(stype: STypeE,labelSubstitution:Map[String,String]): STypeE = {
    stype.map(f => renameLabels(f,labelSubstitution))
  }
  def renameLabels(t: LabelE, labelSubstitution: Map[String, String]): LabelE = t match {
    case lv@LabelVar(y) =>
      if (labelSubstitution.contains(y))
        LabelVar(labelSubstitution(y)).setAstNode(lv.astNode)
      else t
    case ot@ObjectType(y, methods) =>
      ObjectType(y,
         renameLabels(methods,labelSubstitution)).setAstNode(ot.astNode)
    case et@ExistentialType(typeVars,methods) =>
        ExistentialType(
          typeVars.map(tv => EVarDecl(tv.name,renameLabels(tv.lowerBound,labelSubstitution).asInstanceOf[TypeE])),
           renameLabels(methods,labelSubstitution))
    case _ => t

  }
  def renameLabels(methods: List[MethodDeclarationE],
                   labelSubstitution: Map[String, String]): List[MethodDeclarationE] = {
    methods.map(m =>
      MethodDeclarationE(m.name,
        MTypeE(
          m.mType.domain.map(stype => renameLabels(stype,labelSubstitution)),
          renameLabels(m.mType.codomain,labelSubstitution)
        )))
  }


}
object TypeSubstE{
  def renameLabels(selfAscription: STypeE, labelSubstitution: Map[String, String]) = {
    val instance = new TypeSubstE(new AuxiliaryFunctions)
    instance.renameLabels(selfAscription,labelSubstitution)
  }


  def subst(e: LabelE, typeVars: List[String], implTypes: List[TypeE],labelSubstitution:mutable.HashMap[String,String]):LabelE = {
    val instance = new TypeSubstE(new AuxiliaryFunctions)
    typeVars.zip(implTypes).foldLeft(e)((acc,pair)=> 
      instance.subst(acc,pair._1,pair._2,labelSubstitution))
  }


  def subst(t: LabelE, x: String, t2: LabelE,labelSubstitution:mutable.HashMap[String,String]): LabelE = {
    val instance = new TypeSubstE(new AuxiliaryFunctions)
    instance.subst(t,x,t2,labelSubstitution)
  }

}