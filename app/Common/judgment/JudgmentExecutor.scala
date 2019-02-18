package Common.judgment

import models.judgment.GlobalAdapters._

trait JudgmentPredicateEvaluated extends JudgmentPredicate{
  def hold: Boolean
}
object JudgmentPredicateEvaluated {

  case class JudgmentPredicateEvaluatedImp(predicate:JudgmentPredicate,hold:Boolean) extends  JudgmentPredicateEvaluated {
    override def key: String = predicate.key

    override def metaTerms: List[MetaExpr] = predicate.metaTerms

    override def replace(l: List[(Int, MetaExpr)]): JudgmentPredicate = predicate.replace(l)
  }
  def hold(predicate: JudgmentPredicate, hold:Boolean):JudgmentPredicateEvaluated ={
    JudgmentPredicateEvaluatedImp(predicate,hold)
  }
}

trait JudgmentStepEvaluated extends JudgmentBase{
  def premises: List[JudgmentStepEvaluated]
  def sideConditions : List[JudgmentPredicate]
  def state:JudgmentStepState
}
object JudgmentStepEvaluated{
  def fail (goal:JudgmentGoal,
            premises: List[JudgmentStepEvaluated],
            context: JudgmentContext = EmptyJudgmentContext,
            sideConditions:List[JudgmentPredicateEvaluated] = List(),
            output:JudgmentOutput = NoOutput):JudgmentStepEvaluated=
    JudgmentStepEvaluatedImpl(JudgmentStepFailState,goal,premises,context,sideConditions,output)

  def hold(goal:JudgmentGoal,
           premises: List[JudgmentStepEvaluated],
           context: JudgmentContext = EmptyJudgmentContext,
           sideConditions:List[JudgmentPredicateEvaluated] = List(),
           output:JudgmentOutput = NoOutput):JudgmentStepEvaluated=
    JudgmentStepEvaluatedImpl(JudgmentStepHoldState,goal,premises,context,sideConditions,output)

  def noRule(goal:JudgmentGoal,
           premises: List[JudgmentStepEvaluated],
           context: JudgmentContext = EmptyJudgmentContext,
           sideConditions:List[JudgmentPredicateEvaluated] = List(),
           output:JudgmentOutput = NoOutput):JudgmentStepEvaluated=
    JudgmentStepEvaluatedImpl(JudgmentStepNoRuleState,goal,premises,context,sideConditions,output)
}
case class JudgmentStepEvaluatedImpl(state: JudgmentStepState,
                                     goal:JudgmentGoal,
                                     premises: List[JudgmentStepEvaluated],
                                     context: JudgmentContext = EmptyJudgmentContext,
                                     sideConditions:List[JudgmentPredicateEvaluated] = List(),
                                     output:JudgmentOutput = NoOutput) extends JudgmentStepEvaluated{

}

object JudgmentExecutor{
  def stepAll(request:JudgmentRequest): JudgmentStepEvaluated={
    val judgmentAdapter = judgmentAdapters(request.key)
    val step = judgmentAdapter.step(request)

    //create a graph of dependencies between judgment and predicates to know what
    // to evaluate first. For now we have simple approach: to assume that there are
    //only dependencies from sideCondititions to premises and from the step output
    //to premises
    //
    val innerSteps = step.premises.map(premise => {
      val premiseRequest  = JudgmentRequest(premise.context,premise.goal,premise.key)
      stepAll(premiseRequest)
    })
    if(innerSteps.exists(x=>x.state != JudgmentStepHoldState))
      return JudgmentStepEvaluated.fail(step.goal,innerSteps,step.context,List(),step.output)

    //obtain all the output of premises
    val mapPremisesOutputs  = innerSteps.zipWithIndex.
          filter(p=> p._1.output != NoOutput).map(p=> (p._2+1,p._1.output.metaExpr))
    val substitutedSideCondtions = step.sideConditions.map(x=> x.replace(mapPremisesOutputs))

    //evaluate the sideConditions
    val evaluatedSideConditions = substitutedSideCondtions.map(evaluateSideCondition)
    if(evaluatedSideConditions.exists(x=> !x.hold))
      return JudgmentStepEvaluated.fail(step.goal,innerSteps,step.context,List(),step.output)

    //evaluate output if needed.
    val finalOutput = step.output match {
      case outPut: PendingOutput =>  DoneOutput(outPut.metaExpr.replace(mapPremisesOutputs))
      case x => x
    }

    step.state match{
      case JudgmentStepFailState => JudgmentStepEvaluated.fail(step.goal,innerSteps,step.context,List(),step.output)
      case JudgmentStepNoRuleState => JudgmentStepEvaluated.noRule(step.goal,innerSteps,step.context,List(),step.output)
      case JudgmentStepPendingState => JudgmentStepEvaluated.hold(step.goal,innerSteps,step.context,List(),step.output)
      case JudgmentStepHoldState => JudgmentStepEvaluated.hold(step.goal,innerSteps,step.context,List(),step.output)
    }
  }
  def evaluateSideCondition(predicate: JudgmentPredicate): JudgmentPredicateEvaluated = {
    var predicateAdapter = predicateAdapters(predicate.key)
    val childTermEvaluationResults =  predicate.metaTerms.map(x=> evaluateMetaExpr(x))

    //TODO: we should predicate as meta-expression
    val firstError = childTermEvaluationResults.find(p=> p.isInstanceOf[Right[MetaExpr,String]])
    firstError match{
      case Some(e) =>  JudgmentPredicateEvaluated.hold(predicate,hold = false)
      case None =>
        val evaluatedTerms = childTermEvaluationResults.map(x=>x.asInstanceOf[Left[MetaExpr,String]].a)
        val result = predicateAdapter.eval(evaluatedTerms)

        //TODO: comunicate proper errors
        JudgmentPredicateEvaluated.hold(predicate,result match {
          case Left(x) => x
          case _ => false
        })
    }
  }
  def evaluateMetaExpr(m : MetaExpr): Either[MetaExpr,String] ={
    if(m.children.isEmpty)
      Left(m)
    else{
      val evaluatedChild = m.children.map(x=> evaluateMetaExpr(x))
      val firstError = evaluatedChild.find(p=> p.isInstanceOf[Right[MetaExpr,String]])
      firstError match{
        case Some(e) => e
        case None =>
          val evaluatedTerms = evaluatedChild.map(x=>x.asInstanceOf[Left[MetaExpr,String]].a)
          m match{
            case function:MetaFunction =>
              functionAdapters(function.key).eval(evaluatedTerms)
            case _ => Left(m.replace(evaluatedTerms.zipWithIndex.map(p=> (p._2+1,p._1))))
          }
      }
    }
  }
}