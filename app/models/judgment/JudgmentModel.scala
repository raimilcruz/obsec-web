package models.judgment

import Common.judgment._

case class JudgmentRequestApi(key:String,stepId:String,context:JudgmentContext,goal:JudgmentGoal)
case class JudgmentPremiseApi(key:String,premise:JudgmentPremise){
  def toJudgmentPremise:JudgmentPremise= premise
}
object JudgmentPremiseApi{
  def from(key:String,premise:JudgmentPremise):JudgmentPremiseApi= JudgmentPremiseApi(key,premise)
}
case class JudgmentPredicateApi(key:String,predicate:JudgmentPredicate)
object JudgmentPredicateApi{
  def from(key:String,predicate:JudgmentPredicate):JudgmentPredicateApi= JudgmentPredicateApi(key,predicate)
}

case class JudgmentStepApi(key:String,stepId:String,step:JudgmentStep)

/*
  """
      {
        "key": "less-eq"
        context: null,
        goal: {
          n1: 1
          n2: 2
        }
      }
    """
 */

case class JudgmentResumeApi(key:String,stepId:String,context:JudgmentContext,goal:JudgmentGoal,args:List[String])
