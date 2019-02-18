package models.JsonFormatters

import Common.judgment._
import Common.judgment.examples.{LessEqJudgmentGoal, Nat}
import models.judgment.JudgmentRequestApi
import play.api.libs.json._
import play.api.libs.functional.syntax._


class LessEqJudgmentReader  extends JudgmentReader {
  override def key: String = "less-eq"

  /**
    * Deserializer for Nat types.
    */
  implicit object NatReads extends Reads[Nat] {
    def reads(json: JsValue) = json match {
      case JsNumber(n) if n.isValidInt => JsSuccess(Nat.numberToNat(n.toInt))
      case JsNumber(n) => JsError("error.expected.nat")
      case _ => JsError("error.expected.jsnumber")
    }
  }

  implicit object ContextReads extends Reads[JudgmentContext] {
    def reads(json: JsValue) = json match {
      case JsNull => JsSuccess(EmptyJudgmentContext)
      case _ => JsError("error.expected.jsnull")
    }
  }

  implicit val goalReads: Reads[LessEqJudgmentGoal] =
    ((JsPath \ "n1").read[Nat] and
      (JsPath \ "n2").read[Nat]).apply(LessEqJudgmentGoal.apply _)


  override def reads: Reads[JudgmentRequestApi] =
    (
      (JsPath \ "key").read[String] and
        (JsPath \ "stepId").read[String] and
        (JsPath \ "context").read[JudgmentContext] and
        (JsPath \ "goal").read[LessEqJudgmentGoal]
      )(JudgmentRequestApi.apply _)
}

class LessEqJudgmentWriter  extends JudgmentWriter {
  override def key: String = "less-eq"

  implicit object ContextWrites extends Writes[JudgmentContext] {
    override def writes(o: JudgmentContext): JsValue = JsNull
  }

  implicit val goalWrites: Writes[LessEqJudgmentGoal] = new Writes[LessEqJudgmentGoal] {
    override def writes(o: LessEqJudgmentGoal): JsValue = Json.obj(
      "n1" -> o.n1.toInt,
      "n2" -> o.n2.toInt,
      "rep" -> s"${o.n1.toInt} \\leq ${o.n2.toInt}"
    )
  }

  override def premiseWrites: Writes[JudgmentPremise] = new Writes[JudgmentPremise] {
    override def writes(o: JudgmentPremise): JsValue = Json.obj(
      "key" -> key,
      "context" -> o.context,
      "goal" -> o.goal.asInstanceOf[LessEqJudgmentGoal],
      "rep" -> conclusionRep(o)
    )
  }

  override def stepWrites(stepId:String): Writes[JudgmentStep] = JudgmentJsonWriters.judgmentStepWriter(key,stepId)

  override def conclusionRep(judgment: JudgmentBase): String =
    ((g:LessEqJudgmentGoal)=> s"${g.n1.toInt} == ${g.n2.toInt}").apply(judgment.goal.asInstanceOf[LessEqJudgmentGoal])
}
