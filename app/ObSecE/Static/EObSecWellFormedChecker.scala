package ObSecE.Static

import Common._
import ObSecE.Ast._

import scala.collection.mutable

abstract class IEObSecWellFormedChecker(judgements: EObSecJudgements, errors: ErrorCollector)
  extends IJudgment(judgements, errors){

  def isWellFormed(stype: STypeE): Boolean =
    isWellFormed(new Scope[LabelE](), Environment.empty[TypeE](),stype)

  def isWellFormed(typeAliasScope:Scope[LabelE],deltaEVar: Environment[TypeE], stype: STypeE): Boolean

  def isWellFormed(typeAliasScope:Scope[LabelE],deltaEVar: Environment[TypeE], t: LabelE): Boolean

}

/**
  * Implements well-formed judgments for types.
  */
class EObSecWellFormedChecker(judgements: EObSecJudgements, errorCollector: ErrorCollector)
  extends IEObSecWellFormedChecker(judgements,errorCollector) {


  def isWellFormed(typeAliasScope:Scope[LabelE],deltaEVar: Environment[TypeE],
                   stype: STypeE): Boolean =  {
    val closedTypeJudgment = new ClosedTypeJudgment(errorCollector)
    val wellFormedFacetedWiseJudgment = new WellFormedFacetedWise(judgements,errorCollector)
    var soundGlobalPolicies = new SoundGlobalPolicies

    closedTypeJudgment.isClosed(typeAliasScope:Scope[LabelE], deltaEVar.toList.map(x=> x._1).toSet,stype) &&
      wellFormedFacetedWiseJudgment
        .isWellFormedFacetedWise(typeAliasScope:Scope[LabelE],deltaEVar,Environment.empty[ObjectType](),stype)
    //TODO: use soundGlobalPolicies
  }

  def isWellFormed(typeAliasScope:Scope[LabelE],deltaEVar: Environment[TypeE], t: LabelE): Boolean = {
    val closedTypeJudgment = new ClosedTypeJudgment(errorCollector)
    val wellFormedFacetedWiseJudgment = new WellFormedFacetedWise(judgements,errorCollector)
    closedTypeJudgment.isClosed(typeAliasScope,deltaEVar.toList.map(x=> x._1).toSet,t) &&
      wellFormedFacetedWiseJudgment.isWellFormedFacetedWise(typeAliasScope,deltaEVar,Environment.empty[ObjectType](),t)
  }
}

//judgment ∆_OK |- S
class ClosedTypeJudgment(errorCollector: ErrorCollector){
  def isClosed(typeAliasScope:Scope[LabelE],deltaOk:Set[String], stype: STypeE): Boolean = stype match{
    case FTypeE(priv,pub)=> isClosed(typeAliasScope,deltaOk,priv) && isClosed(typeAliasScope,deltaOk,pub)
    case ESTypeE(priv,impl,pub)=>
      isClosed(typeAliasScope,deltaOk,priv) &&
        impl.forall(x=>isClosed(typeAliasScope,deltaOk,x)) && isClosed(typeAliasScope,deltaOk,pub)
  }


  def isClosed(typeAliasScope:Scope[LabelE],deltaOk:Set[String], theType: LabelE): Boolean = theType match{
    case TypeVar(id) => deltaOk.contains(id)
    case LabelVar(id) => deltaOk.contains(id)
    case TypeId(id) => typeAliasScope.contains(id)
    case obj@ObjectType(x, methods) =>
      isClosed(typeAliasScope,deltaOk + x,methods)
    case ex@ExistentialType(typeVars,methods) =>
      isClosed(typeAliasScope,deltaOk ++ typeVars.map(x=>x.name).toSet,methods)
    case _ => true
  }
  def isClosed(typeAliasScope:Scope[LabelE],deltaOk: Set[String],methods:List[MethodDeclarationE]):Boolean= {
    methods
      .forall(m => {
          m.mType
            .domain
            .forall(s =>
              isClosed(typeAliasScope,deltaOk, s)) &&
            isClosed(typeAliasScope,deltaOk, m.mType.codomain)
      })
  }
}

class WellFormedFacetedWise(judgements: EObSecJudgements,errorCollector: ErrorCollector){
  private val validRelationBetweenFacets = new ValidRelationBetweenFacets(judgements,errorCollector)

  def isWellFormedFacetedWise(typeAliasScope:Scope[LabelE],deltaSub: Environment[TypeE], oTypeEnv: Environment[ObjectType],
                              stype:STypeE):Boolean = stype match{
    case FTypeE(priv,pub) =>
      isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,priv) &&
        isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,pub) &&
        validRelationBetweenFacets.subtypingRel(typeAliasScope,deltaSub,
          closeType(oTypeEnv,priv),
          closeType(oTypeEnv,pub))|| {
          throw CommonError.facetedWiseSubtypingError(stype.astNode,stype.prettyPrint())
        }
    case eft@ESTypeE(priv,impl,pub) =>
      isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,priv) &&
        impl.forall(x=> isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,x)) &&
        isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,pub) &&
          validRelationBetweenFacets.existentialRel(typeAliasScope,
            Environment.empty[TypeE](),
            eft.map(x=> closeType(oTypeEnv,x)))

  }

  def isWellFormedFacetedWise(typeAliasScope:Scope[LabelE],deltaSub: Environment[TypeE],oTypeEnv: Environment[ObjectType],
                              t:LabelE):Boolean = t match {
    case ot@ObjectType(self,methods) =>
      isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv.extend(self,ot),methods.map(m=> m.mType))
    case et@ExistentialType(typeVars,methods)=>
      val extendEnv = judgements.auxiliaryDefinitions.multiExtend(deltaSub,typeVars.map(x=> (x.name,x.lowerBound)))
      isWellFormedFacetedWise(typeAliasScope,extendEnv ,oTypeEnv,methods.map(m=> m.mType))
    case _ => true
  }

  def isWellFormedFacetedWise(typeAliasScope:Scope[LabelE],deltaSub: Environment[TypeE], oTypeEnv: Environment[ObjectType],
                              methods:List[MTypeE]):Boolean =
    methods.forall(m => isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,m))

  def isWellFormedFacetedWise(typeAliasScope:Scope[LabelE], deltaSub: Environment[TypeE],oTypeEnv: Environment[ObjectType],
                              mType:MTypeE):Boolean =
    mType.domain.forall(t=> isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,t)) &&
      isWellFormedFacetedWise(typeAliasScope,deltaSub,oTypeEnv,mType.codomain)

  private def closeType(env: Environment[ObjectType], t: LabelE): LabelE =
    if (env.isEmpty) t
    else {
      val head = env.head
      closeType(env.tail, TypeSubstE.subst(t, head._1, head._2,mutable.HashMap[String,String]()))
    }
}
class ValidRelationBetweenFacets(judgements: EObSecJudgements, errorCollector: ErrorCollector){

  def subtypingRel(typeAliasScope:Scope[LabelE], deltaEVar:Environment[TypeE],  priv: LabelE, public: LabelE):Boolean={
     judgements.<::(typeAliasScope,deltaEVar,priv,public) == SubtypingSuccess
  }



  def existentialRel(typeAliasScope:Scope[LabelE], deltaEVar: Environment[TypeE], ef:ESTypeE): Boolean = ef match {
    case ESTypeE(priv: TypeE, impl: List[TypeE], existentialFacet) =>
      val existential = liftToExistentialType(typeAliasScope,existentialFacet)
      var record = TypeSubstE.subst(
        RecordTypeE(existential.methods),
        existential.typeVars.map(x=>x.name),
        impl,mutable.HashMap[String,String]())
      //verify:
      //1. The implementation types are super type of the lower bounds
      if(impl.size!=existential.typeVars.size)
        throw EObSecWellFormednessError.invalidExistentialFacet(ef.astNode,"Amount of implementation types must" +
          "be equal to amount of existential variables")
      impl.zip(existential.typeVars.map(x=>x.lowerBound))
        .foreach {
          case (implType,lowerBound)=>
            if(judgements.<::(typeAliasScope,deltaEVar,lowerBound,implType) != SubtypingSuccess){
              throw EObSecWellFormednessError.invalidExistentialFacet(implType.astNode,"Implementation type is not super type " +
                "of the lower bound of the existential variable")
            }
        }

      //2. The concrete type is subtyping of the existential record after substituting type variables
      if(judgements.<::(typeAliasScope,deltaEVar,priv,record) != SubtypingSuccess) {
        throw EObSecWellFormednessError.invalidExistentialFacet(ef.astNode, "The private type is not subtype of " +
          "the public type after closing the public type with the implementation types")
      }
      true
    case _ => throw new NotImplementedError("type cast error in existentialRel")
  }
  private def liftToExistentialType(typeAliasScope: Scope[LabelE],
                                    existentialFacet: ExistentialFacet):ExistentialType = existentialFacet match{
    case et:ExistentialType=> et
    case TypeId(name)=> typeAliasScope.lookup(name).asInstanceOf[ExistentialType]
  }
}
