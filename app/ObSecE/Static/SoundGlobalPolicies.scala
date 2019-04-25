package ObSecE.Static

import ObSecE.Ast._

class SoundGlobalPolicies {



  def soundSignature(integrityScope: Set[STypeE], m: MTypeE): Boolean = {
    m.domain.forall(t=> soundType(integrityScope,t)) &&
      soundType(integrityScope,m.codomain) &&
      soundScope(integrityScope)
  }
  def soundType(integrityScope: Set[STypeE], t: STypeE): Boolean = t match{
    case FTypeE(privateType,publicType) =>
        soundType(integrityScope,privateType) &&
          soundType(integrityScope,publicType)
    case ESTypeE(privateType,implTypes,existentialFacet) =>
      soundType(integrityScope,privateType) &&
        soundType(integrityScope,existentialFacet) &&
          implTypes.forall(it => soundType(integrityScope,it))
    case _ => true
  }
  def soundType(integrityScope: Set[STypeE], t: LabelE): Boolean = t match{
    case ObjectType(self,methods) =>
      methods.forall(m=> soundSignature(integrityScope,m.mType))
    case ExistentialType(typeVars,methods) =>
      methods.forall(m=> soundSignature(integrityScope,m.mType))
    case _ => true
  }


  def soundScope(integrityScope: Set[STypeE]): Boolean =
    integrityScope.forall(isGlobalSecret) && integrityScope.forall(isNotGlobalSecret)

  private def isGlobalSecret(t:STypeE):Boolean = t match {
    case FTypeE(priv,x:LabelVar) => x.isAster
    case _ => false
  }
  private def isNotGlobalSecret(t:STypeE):Boolean= !isGlobalSecret(t)
  private def extendIntegrityScope(scope:Set[STypeE],t:STypeE): Set[STypeE] =  t  match{
    case FTypeE(privateType,ObjectType(self,methods)) =>
      methods.map(x=>x.mType.codomain).foldLeft(scope)((acc,x)=> extendIntegrityScope(acc,x))
    case ESTypeE(privateType,impl,ExistentialType(typeVars,methods)) =>
      methods.map(x=>x.mType.codomain).foldLeft(scope)((acc,x)=> extendIntegrityScope(acc,x))
    case _ =>
      scope + t
  }


}
