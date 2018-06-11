package ObSecG.Static

import Common.{Environment, ErrorCollector, Scope}
import ObSecG.Ast._
import ObSecG.Static.TypeEquivalenceG.recAlphaEq


trait Environments{
  type LabelVarEnvironment = Environment[TypeVarBounds]
  type TypeEnvironment = Scope[STypeG]
  type TypeAliasScope = Scope[TypeG]
  type SelfDefinitionEnvironment = Environment[ObjectType]
  type SelfVariableSet = Set[TypeVar]
  type SubtypingAssumptions = Set[Tuple2[LabelG, LabelG]]
}

trait GObSecJudgements extends Environments{

  def isWellFormed(labelVarEnvironment: LabelVarEnvironment,
                   stype: LabelG):Boolean

  /**
    * Type checker judgement
    *
    * @param labelVariableEnvironment The label variable environment
    * @param scope The value environment
    * @param aliasScope The alias definition environment
    * @param expr The expression to typecheck
    */
  def typeCheck(labelVariableEnvironment: Environment[TypeVarBounds],
                scope: TypeEnvironment,
                aliasScope: TypeAliasScope,
                expr: ObSecGExpr): STypeG

  /**
    * Determine if two label types are equivalent
    * @param t1
    * @param t2
    * @return
    */
  def alphaEq(t1:LabelG,t2:LabelG):Boolean


  def <::(labelVariableEnv:LabelVarEnvironment,  t1: LabelG, t2: LabelG):Boolean

  def auxiliaryDefinitions: IAuxiliaryFunctions

}

trait GObSecJudgmentsExtensions extends GObSecJudgements{
  def isWellFormed(labelVarEnvironment: LabelVarEnvironment,
                   stype: STypeG):Boolean
    = isWellFormed(labelVarEnvironment,stype.privateType) &&
     isWellFormed(labelVarEnvironment,stype.publicType)


  def <::(labelVariableEnv:LabelVarEnvironment, s1:STypeG,s2:STypeG):Boolean =
    <::(labelVariableEnv,s1.privateType, s2.privateType) &&
      <::(labelVariableEnv,s1.publicType, s2.publicType)


}

class GObSecGJudgmentImpl(val errorCollector: ErrorCollector) extends GObSecJudgmentsExtensions {
  /**
    * Type checker judgement
    *
    * @param labelVariableEnvironment The label variable environment
    * @param scope                    The value environment
    * @param aliasScope               The alias definition environment
    * @param expr                     The expression to typecheck
    */
  override def typeCheck(labelVariableEnvironment: Environment[TypeVarBounds],
                         scope: TypeEnvironment,
                         aliasScope: TypeAliasScope,
                         expr: ObSecGExpr): STypeG = {

    var typeChecker = new TypeChecker(this,errorCollector)
    typeChecker.typeCheck(labelVariableEnvironment,scope,aliasScope,expr)
  }

  /**
    * Determine if two label types are equivalent
    *
    * @param t1
    * @param t2
    * @return
    */
  override def alphaEq(t1: LabelG, t2: LabelG): Boolean = ???

  override def isWellFormed(labelVarEnvironment: LabelVarEnvironment, theType: LabelG): Boolean = {
    val wellFormedChecker = new WellFormedCheckerG(this,errorCollector)
    wellFormedChecker.isWellFormed(labelVarEnvironment,theType)
  }

  override def <::(labelVariableEnv: LabelVarEnvironment, t1: LabelG, t2: LabelG): Boolean = {
    var subtyping = new AmadioCardelliSubtypingG(this,errorCollector)
    subtyping.<::(labelVariableEnv,t1,t2)
  }

  override def auxiliaryDefinitions: IAuxiliaryFunctions = new AuxiliaryFunctions
}

abstract class IJudgment(judgements: GObSecJudgements,
                         errors: ErrorCollector) extends Environments{
}



