package controllers

import javax.inject._

import Common.judgment._
import Common.judgment.examples._
import Common.judgment.examples.lambdacalculus.{TypingEnvironment, TypingGoal, TypingPremise}
import models.JsonFormatters.JudgmentJsonWriters._
import models.JsonFormatters.Typing.LambdaCalculusParser
import models.judgment.GlobalAdapters._
import models.judgment.JudgmentPremiseApi
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers, Controller}


@Singleton
class ExampleJudgmentController @Inject()(configuration: play.api.Configuration) extends Controller {
  //Entrance Door to judgment machinery for the LessEq judgment
  case class JudgmentRequestLessEq(n1:String,n2:String)
  case class JudgmentRequestTyping(context:String,expr: String)

  implicit val jrEqReads = Json.reads[JudgmentRequestLessEq]
  implicit val jrTypingReads = Json.reads[JudgmentRequestTyping]


  def getJudgment = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[JudgmentRequestLessEq]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        if(!judgmentAdapters.contains("less-eq"))
          Ok(Json.obj("status" -> "AnalysisKO","error" ->s"judgment less-eq not understood"))
        else {
          Ok(Json.obj("status" -> "OK","premise" ->
            JudgmentPremiseApi.from("less-eq", LessEqPremise(EmptyJudgmentContext,
              LessEqJudgmentGoal(Nat.numberToNat(f.n1.toInt), Nat.numberToNat(f.n2.toInt))))))
        }
      }
    )
  }
  def getTypingJudgment = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[JudgmentRequestTyping]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        if(!judgmentAdapters.contains("typing"))
          Ok(Json.obj("status" -> "AnalysisKO","error" ->s"judgment less-eq not understood"))
        else {
          LambdaCalculusParser(f.expr) match{
            case Right(t) =>
              Ok(Json.obj("status" -> "OK","premise" ->
                      JudgmentPremiseApi.from("typing", TypingPremise(new TypingEnvironment(),TypingGoal(t)))))
            case Left(e) =>  Ok(Json.obj("status" -> "AnalysisKO","error" ->s"Error parsing expression: ${f.expr}. Error: $e"))
          }
        }
      }
    )
  }
  def stepJudgment = Action(BodyParsers.parse.json) { implicit request =>
    throw new NotImplementedError()
  }

}









