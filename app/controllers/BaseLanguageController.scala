package controllers

import Common.{AstNode, ParserError, ThrowableAnalysisError}
import models._
import play.api.libs.json.{Json, _}
import play.api.mvc._

abstract class BaseLanguageController (configuration: play.api.Configuration) extends Controller {

  implicit val fReads = Json.reads[Program]
  implicit val errorPositionFormat = Json.format[UIErrorPosition]
  implicit val analysisErrorFormat = Json.format[UIAnalysisError]
  implicit val exampleFormat = Json.format[Example]


  def examples = Action { implicit request =>
    val l = getExamples
    Ok(Json.obj("status" -> "OK", "examples" -> l))
  }

  def typecheck = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[Program]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        parse(f.program) match {
          case Right(term) =>
            try {
              val typeRep = typeOf(term)
              Ok(Json.obj("status" -> "OK", "program" -> f.program,"expressionType"-> typeRep))
            } catch {
              case te : ThrowableAnalysisError =>
                Ok(Json.obj("status" -> "AnalysisKO", "issue" -> CommonModel.analysisErrorToUi(te.analysisError)))
              case e: Throwable =>
                print("error")
                print(e)
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }
          case Left(error) => Ok(Json.obj("status" -> "AnalysisKO", "issue" -> CommonModel.parseErrorToUi(error)))
        }
      }
    )
  }

  def reduce = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[Program]
    fResult.fold(
      errors => {
        print("Bad request:")
        println(fResult)
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        parse(f.program) match {
          case Right(term) =>
            try {
              val result = run(term)
              Ok(Json.obj("status" -> "OK",
                "program" -> f.program,
                "result"-> result.toString
              ))
            }
            catch {
              case e: Throwable =>
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }
          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> "Parser error"))
        }
      }
    )
  }
  protected def getExamples : List[Example]
  protected def parse(p:String): Either[ParserError, AstNode]
  protected def typeOf(node:AstNode): String
  protected def run(node:AstNode): String
}
