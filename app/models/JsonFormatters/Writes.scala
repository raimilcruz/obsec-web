package models.JsonFormatters

import Common.judgment._
import controllers.{JudgmentPremiseApi, JudgmentStepApi}
import play.api.libs.json._

//Json Writers
object JudgmentJsonWriters{
  val judgmentWriters : Map[String,JudgmentWriter] =
    Map("less-eq" ->  new LessEqJudgmentWriter,
      "eq" ->  new EqJudgmentWriter)

  implicit val premiseWrites = new Writes[JudgmentPremiseApi] {
    def writes(premise: JudgmentPremiseApi): JsValue =
      if(!judgmentWriters.contains(premise.key))
        Json.obj(
          "error" -> s"error.notpremisewrite${premise.key}"
        )
      else
        judgmentWriters(premise.key).premiseWrites.writes(premise.toJudgmentPremise)
  }
  implicit val stepWrites = new Writes[JudgmentStepApi] {
    def writes(stepApi: JudgmentStepApi): JsValue =
      if(!judgmentWriters.contains(stepApi.key))
        Json.obj(
          "error" -> s"error.notpremisewrite.${stepApi.key}"
        )
      else
        judgmentWriters(stepApi.key).stepWrites(stepApi.stepId).writes(stepApi.step)
  }

  def judgmentStepWriter(stepId:String) = new Writes[JudgmentStep]{
    override def writes(o: JudgmentStep): JsValue = o match{
      case x:JudgmentStepBaseCase => baseCaseStepWrites(stepId).writes(x)
      case x:JudgmentStepRecursiveCase => recursiveStepWrites(stepId).writes(x)
      case x:JudgmentStepNoRuleCase => noRuleCaseWrites(stepId).writes(x)
    }
  }

  def baseCaseStepWrites(stepId:String) : Writes[JudgmentStepBaseCase] =
    new Writes[JudgmentStepBaseCase]{
      def writes(goal: JudgmentStepBaseCase): JsObject = Json.obj(
        "kind" -> "case-base-rule"
      )
    }

  def recursiveStepWrites(stepId:String): Writes[JudgmentStepRecursiveCase] = new Writes[JudgmentStepRecursiveCase]{
    def writes(goal: JudgmentStepRecursiveCase): JsObject = Json.obj(
      "kind" -> "pending-rule",
      "premises" -> JsArray(goal.premises.map(x=>
        premiseWrites.writes(JudgmentPremiseApi.from(x.key,x))))
    )
  }

  def noRuleCaseWrites(stepId:String) :Writes[JudgmentStepNoRuleCase]= new Writes[JudgmentStepNoRuleCase]{
    def writes(goal: JudgmentStepNoRuleCase): JsObject = Json.obj(
      "kind" -> "no-rule"
    )
  }
}

trait JudgmentWriter{
  def key:String
  def premiseWrites : Writes[JudgmentPremise]
  def stepWrites(stepId:String) : Writes[JudgmentStep]
}
