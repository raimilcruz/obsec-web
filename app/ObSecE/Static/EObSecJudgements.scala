package ObSecE.Static

import Common._
import ObSecE.Ast._



trait EObSecJudgements {

  def isWellFormed(typeAliasScope:Scope[LabelE], deltaEVar: Environment[TypeE],stype: LabelE):Boolean
  def isWellFormed(typeAliasScope:Scope[LabelE], deltaEVar: Environment[TypeE], stype: STypeE):Boolean

  def typeCheck(deltaInst: Environment[TypeE],
                scope:  Scope[STypeE],
                aliasScope:  Scope[LabelE],
                expr: EObSecExpr): STypeE

  def alphaEq(t1:LabelE,t2:LabelE):Boolean

  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE],  t1: LabelE, t2: LabelE):SubtypingResult

  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE], s1:STypeE,s2:STypeE):SubtypingResult


  def auxiliaryDefinitions: IAuxiliaryFunctions

}

class EObSecGJudgmentImpl(val errorCollector: ErrorCollector) extends EObSecJudgements {
  /**
    * Type checker judgement
    *
    * @param scope                    The value environment
    * @param aliasScope               The alias definition environment
    * @param expr                     The expression to typecheck
    */
  override def typeCheck(deltaInst: Environment[TypeE],
                         scope:  Scope[STypeE],
                         aliasScope:  Scope[LabelE],
                         expr: EObSecExpr): STypeE = {

    var typeChecker = new EObSecTypeChecker(this,errorCollector)
    typeChecker.typeCheck(deltaInst,scope,aliasScope,expr)
  }

  /**
    * Determine if two label types are equivalent
    *
    * @param t1
    * @param t2
    * @return
    */
  override def alphaEq(t1: LabelE, t2: LabelE): Boolean = ???

  def isWellFormed(typeAliasScope:Scope[LabelE],deltaEVar: Environment[TypeE],
                   stype: STypeE):Boolean   = {
    val wellFormedChecker = new EObSecWellFormedChecker(this, errorCollector)
    wellFormedChecker.isWellFormed(typeAliasScope,deltaEVar, stype)
  }

  override def isWellFormed(typeAliasScope:Scope[LabelE],
                            deltaEVar: Environment[TypeE], theType: LabelE): Boolean = {
    val wellFormedChecker = new EObSecWellFormedChecker(this, errorCollector)
    wellFormedChecker.isWellFormed(typeAliasScope,deltaEVar, theType)
  }

  override def <::(typeAliasScope:Scope[LabelE],deltaInst: Environment[TypeE], t1: LabelE, t2: LabelE): SubtypingResult = {
    val amadioCardelliSubtypingEObsec = new AmadioCardelliSubtypingEObsec(this,errorCollector)
    amadioCardelliSubtypingEObsec.<::(typeAliasScope:Scope[LabelE],deltaInst,t1,t2)
  }
  def <::(typeAliasScope:Scope[LabelE],deltaInst:Environment[TypeE], s1:STypeE,s2:STypeE):SubtypingResult={
    val amadioCardelliSubtypingEObsec = new AmadioCardelliSubtypingEObsec(this,errorCollector)
    amadioCardelliSubtypingEObsec.<::(typeAliasScope:Scope[LabelE],deltaInst,s1,s2)
  }

  override def auxiliaryDefinitions: IAuxiliaryFunctions = new AuxiliaryFunctions
}

abstract class IJudgment(judgements: EObSecJudgements,
                         errors: ErrorCollector){

  def wrapError[T](computation: => T,node:AstNode): T ={
    try{
      computation
    }
    catch {
      case e  if !e.isInstanceOf[ThrowableAnalysisError] =>
        //print(e.getStackTraceString)
        throw CommonError.implementationError(node,e.getMessage)
    }
  }
}



