package Common.judgment

trait JudgmentContext{
  //what is common to judgment context?
  //typically we have different kinds of mappings and sets.
  def judgmentKey:String
}

object EmptyJudgmentContext extends JudgmentContext{
  override def judgmentKey: String = "all"
}

trait JudgmentGoal{
  def judgmentKey:String
}
trait JudgmentOutput {
  def mathify:String
  def metaExpr : MetaExpr
}

object NoOutput extends JudgmentOutput {
  override def mathify: String = ""
  override def metaExpr: MetaExpr = ???
}

/*
The output references a pending output (in premises)
 */
case class PendingOutput(metaExpr:MetaExpr) extends JudgmentOutput {
  override def mathify: String = metaExpr.mathify
}
case class DoneOutput(metaExpr:MetaExpr) extends JudgmentOutput {
  override def mathify: String = metaExpr.mathify
}

trait MetaExpr{
  def mathify:String
  def toSource: String
  def children : List[MetaExpr]

  /**
    * Returns a new MetaExpr where dependencies of the form $i where replaced
    * @param l A binding list
    * @return
    */
  def replace(l:List[(Int,MetaExpr)]): MetaExpr
}
trait MetaFunction{
  def key:String
}
/*
Represents the output of another local judgment. A judgment A is local to another
judgment B:
- if A is in the premises of B.
- if A is a sibling premise of B.
The same notion applies between judgment and predicates.

 */
case class JudgmentOutputRef(premIndex: Int) extends MetaExpr {
  override def mathify: String = "$"+s"$premIndex"

  override def children: List[MetaExpr] = List()

  override def toSource: String = "$"+s"$premIndex"

  override def replace(l: List[(Int, MetaExpr)]): MetaExpr = {
    val f = l.find(p=>p._1 == premIndex)
    f match{
      case None => this
      case Some(x)=> x._2
    }
  }
}

case class JudgmentRequest(ctx:JudgmentContext,goal: JudgmentGoal,key:String)



trait JudgmentPredicate{
  def key:String
  def metaTerms: List[MetaExpr]
  def replace(l:List[(Int,MetaExpr)]):JudgmentPredicate
}
trait JudgmentConclusion{
  def context: JudgmentContext
  def goal : JudgmentGoal
}
case class DefaultJudgmentConclusion(context:JudgmentContext,goal:JudgmentGoal) extends JudgmentConclusion

trait JudgmentBase{
  //def conclusion:JudgmentConclusion
  def context: JudgmentContext
  def goal : JudgmentGoal
  def output:JudgmentOutput
}
trait JudgmentPremise extends JudgmentBase{
  //TODO: Do this property complicated the api for the judgment's implementor?
  def key :String
}
sealed trait JudgmentStepState

object JudgmentStepPendingState extends JudgmentStepState
object JudgmentStepHoldState extends JudgmentStepState
object JudgmentStepFailState extends JudgmentStepState
object JudgmentStepNoRuleState extends JudgmentStepState

trait JudgmentStep extends JudgmentBase{
  def premises: List[JudgmentPremise]
  def sideConditions: List[JudgmentPredicate]
  def state:JudgmentStepState
}

object JudgmentStep{
  def base(goal:JudgmentGoal,
           context: JudgmentContext = EmptyJudgmentContext,
           output:JudgmentOutput = NoOutput):JudgmentStep = {
    JudgmentStepBaseCase(goal,context,output)
  }
}

//TODO: Define a notion a JudgmentStepEvaluated that has other JudgmentStepEvaluated
//as premises and JudgmentPredicateEvaluated as sideConditions.


//&&&&&&&&&&&&&&&&&&&&&&&&&&&&
//Implementations

case class JudgmentStepBaseCase(goal:JudgmentGoal,
                                context: JudgmentContext = EmptyJudgmentContext,
                                output:JudgmentOutput = NoOutput) extends JudgmentStep{
  override def premises: List[JudgmentPremise] = List()
  override def sideConditions: List[JudgmentPredicate] = List()

  //override def conclusion: JudgmentConclusion = DefaultJudgmentConclusion(context,goal)
  override def state: JudgmentStepState = JudgmentStepHoldState
}
case class JudgmentStepNoRuleCase(goal: JudgmentGoal,
                                  context: JudgmentContext = EmptyJudgmentContext,
                                  output:JudgmentOutput = NoOutput) extends JudgmentStep{
  override def premises: List[JudgmentPremise] = List()
  override def sideConditions: List[JudgmentPredicate] = List()

  override def state: JudgmentStepState = JudgmentStepNoRuleState
  //override def conclusion: JudgmentConclusion = DefaultJudgmentConclusion(context,goal)
}
case class JudgmentStepRecursiveCase(goal:JudgmentGoal,
                                     premises: List[JudgmentPremise],
                                     context: JudgmentContext = EmptyJudgmentContext,
                                     sideConditions:List[JudgmentPredicate] = List(),
                                     output:JudgmentOutput = NoOutput) extends JudgmentStep{

  //override def conclusion: JudgmentConclusion = DefaultJudgmentConclusion(context,goal)
  override def state: JudgmentStepState = JudgmentStepPendingState
}