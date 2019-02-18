package controllers

import javax.inject._

import Common.judgment._
import Common.judgment.examples.{EqJudgmentAdapter, LessEqJudgmentAdapter}
import Common.judgment.examples.lambdacalculus.TypingJudgmentAdapter
import models.JsonFormatters.JudgmentJsonReaders._
import models.JsonFormatters.JudgmentJsonWriters._
import models.judgment.JudgmentStepApi
import models.judgment.{JudgmentRequestApi, JudgmentStepApi}
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers, Controller}
import models.judgment.GlobalAdapters._


@Singleton
class JudgmentController @Inject()(configuration: play.api.Configuration) extends Controller {

  def stepJudgment = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[JudgmentRequestApi]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        if(!judgmentAdapters.contains(f.key))
          Ok(Json.obj("status" -> "AnalysisKO","error" ->s"judgment ${f.key} not understood"))
        else {
          val res = judgmentAdapters(f.key).step(JudgmentRequest(f.context,f.goal,f.key))
          Ok(Json.obj("status" -> "OK","step" -> JudgmentStepApi(f.key,f.stepId,res)))
        }
      }
    )
  }
  def resumeJudgment = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[JudgmentRequestApi]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        if(!judgmentAdapters.contains(f.key))
          Ok(Json.obj("status" -> "AnalysisKO","error" ->s"judgment ${f.key} not understood"))
        else {
          val res = judgmentAdapters(f.key).step(JudgmentRequest(f.context,f.goal,f.key))
          Ok(Json.obj("status" -> "OK","step" -> JudgmentStepApi(f.key,f.stepId,res)))
        }
      }
    )
  }
}







