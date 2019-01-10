package Common.judgment

trait JudgmentContext{
  //what is common to judgment context?
  //typically we have different kinds of mappings and sets.
  def judgmentKey:String
}

object EmptyJudgmentContext extends JudgmentContext {
  override def judgmentKey: String = "all"
}

trait JudgmentGoal{
  def judgmentKey:String
}

case class JudgmentRequest(ctx:JudgmentContext,goal: JudgmentGoal,key:String)

trait JudgmentAdapter{
  def key:String
  def step(request: JudgmentRequest): JudgmentStep
}

trait JudgmentPredicate{
  def holds: Boolean
}

trait JudgmentBase{
  def context: JudgmentContext
  def goal : JudgmentGoal
}
trait JudgmentPremise extends JudgmentBase{
  //TODO: Do this property complicated the api for the judgment's implementor?
  def key :String
}

trait JudgmentStep extends JudgmentBase{
  def premises: List[JudgmentPremise]
  def sideConditions: List[JudgmentPredicate]
}


//&&&&&&&&&&&&&&&&&&&&&&&&&&&&
//Implementations

case class JudgmentStepBaseCase(goal:JudgmentGoal) extends JudgmentStep{
  override def premises: List[JudgmentPremise] = List()
  override def sideConditions: List[JudgmentPredicate] = List()
  override def context: JudgmentContext = EmptyJudgmentContext
}
case class JudgmentStepNoRuleCase(goal: JudgmentGoal) extends JudgmentStep{
  override def premises: List[JudgmentPremise] = List()
  override def sideConditions: List[JudgmentPredicate] = List()
  override def context: JudgmentContext = EmptyJudgmentContext
}
case class JudgmentStepRecursiveCase(goal:JudgmentGoal, prem: List[JudgmentPremise]) extends JudgmentStep{
  override def premises: List[JudgmentPremise] = prem
  override def sideConditions: List[JudgmentPredicate] = List()
  override def context: JudgmentContext = EmptyJudgmentContext
}