package models.JsonFormatters

import Common.judgment._
import models._
import models.JsonFormatters.Typing.TypingJudgmentEqTypeWriter
import models.judgment.{JudgmentPredicateApi, JudgmentPremiseApi, JudgmentStepApi}
import play.api.libs.json._

//Json Writers
object JudgmentJsonWriters{
  val judgmentWriters : Map[String,JudgmentWriter] =
    Map("less-eq" ->  new LessEqJudgmentWriter,
      "eq" ->  new EqJudgmentWriter,
      "typing" -> new Typing.TypingJudgmentWriter
    )
  val sideConditionWriters : Map[String,SideConditionWriter] =
    Map("lambda-calculus-type-eq" ->  new TypingJudgmentEqTypeWriter)

  implicit val premiseWrites = new Writes[JudgmentPremiseApi] {
    def writes(premise: JudgmentPremiseApi): JsValue =
      if(!judgmentWriters.contains(premise.key))
        Json.obj(
          "error" -> s"error.notpremisewrite${premise.key}"
        )
      else
        judgmentWriters(premise.key).premiseWrites.writes(premise.toJudgmentPremise)
  }
  implicit val sideConditionWrites = new Writes[JudgmentPredicateApi] {
    def writes(predicate: JudgmentPredicateApi): JsValue =
      if(!sideConditionWriters.contains(predicate.key))
        Json.obj(
          "error" -> s"error.notpremisewrite${predicate.key}"
        )
      else
        sideConditionWriters(predicate.key).writes.writes(predicate.predicate)
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

  def judgmentStepWriter(key:String, stepId:String) = new Writes[JudgmentStep]{
    override def writes(o: JudgmentStep): JsValue = (o match{
      case x:JudgmentStepBaseCase => baseCaseStepWrites(stepId).writes(x)
      case x:JudgmentStepRecursiveCase => recursiveStepWrites(stepId).writes(x)
      case x:JudgmentStepNoRuleCase => noRuleCaseWrites(stepId).writes(x)
    }).asInstanceOf[JsObject].++(commonStepWrites(key,stepId).writes(o).asInstanceOf[JsObject])
  }

  def baseCaseStepWrites(stepId:String) : Writes[JudgmentStepBaseCase] =
    new Writes[JudgmentStepBaseCase]{
      def writes(step: JudgmentStepBaseCase): JsObject = Json.obj(
        "kind" -> "case-base-rule"
      )
    }

  def recursiveStepWrites(stepId:String): Writes[JudgmentStepRecursiveCase] = new Writes[JudgmentStepRecursiveCase]{
    def writes(step: JudgmentStepRecursiveCase): JsObject = Json.obj(
      "kind" -> "pending-rule"
    )
  }

  def noRuleCaseWrites(stepId:String) :Writes[JudgmentStepNoRuleCase]= new Writes[JudgmentStepNoRuleCase]{
    def writes(goal: JudgmentStepNoRuleCase): JsObject = Json.obj(
      "kind" -> "no-rule"
    )
  }

  def commonStepWrites(key:String,stepId:String): Writes[JudgmentStep] = new Writes[JudgmentStep]{
    def writes(step: JudgmentStep): JsObject = Json.obj(
      "premises" -> JsArray(step.premises.map(x=>
        premiseWrites.writes(JudgmentPremiseApi.from(x.key,x)))),
      "sideConditions" -> JsArray(step.sideConditions.map(x=>
        sideConditionWrites.writes(JudgmentPredicateApi.from(x.key,x)))),
      "output"->step.output.mathify,
      "rep" -> judgmentWriters(key).conclusionRep(step)
    )
  }
}

trait JudgmentWriter{
  def key:String
  def premiseWrites : Writes[JudgmentPremise]
  def stepWrites(stepId:String) : Writes[JudgmentStep]
  def conclusionRep(judgment: JudgmentBase):String
}
trait SideConditionWriter{
  def key:String
  def writes: Writes[JudgmentPredicate]
}
