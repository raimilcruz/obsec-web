package controllers

import javax.inject._

import Common.{AnalysisError, ThrowableAnalysisError}
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.mvc._
import play.api.libs.json._
import ObSecG.Static.TypeCheckerG
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG


@Singleton
class ApplicationController extends Controller {

  case class Program(program: String)

  implicit val fReads = Json.reads[Program]

  private def normalizeNumber(n:Int):Int={
    if(n>0) n-1
    else 0
  }

  case class UIAnalysisError(kind:String, message: String,position: UIErrorPosition)
  case class UIErrorPosition(line: Int, columnStart: Int, lineEnd: Int, columnEnd: Int)

  def analysisErrorToUi(analysis:AnalysisError,kind:String = "error"):UIAnalysisError ={
    UIAnalysisError(
      kind,
      analysis.toString,
      UIErrorPosition(
        normalizeNumber(analysis.node.pos.line),
        normalizeNumber(analysis.node.pos.column),
        normalizeNumber(analysis.node.endPos.line),
        normalizeNumber(analysis.node.endPos.column)))
  }
  def parseErrorToUi(error:ObSecParserError):UIAnalysisError ={
    UIAnalysisError(
      "parser error",
      error.msg,
      UIErrorPosition(
        normalizeNumber(error.pos.line),
        normalizeNumber(error.pos.column),
        normalizeNumber(error.endPos.line),
        normalizeNumber(error.endPos.column)))
  }


  implicit val errorPositionFormat = Json.format[UIErrorPosition]
  implicit val analysisErrorFormat = Json.format[UIAnalysisError]


  def index(prod: Int) = Action { implicit request =>
    Ok(views.html.index(prod))
  }

  def typecheck = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[Program]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        ObSecGParser(f.program) match {
          case Right(term) =>
            try {
              var modelTerm = ObSecGIdentifierResolver(term)
              val aType = TypeCheckerG(modelTerm)
              val stringBuilder = new StringBuilder
              aType.prettyPrint(stringBuilder)
              Ok(Json.obj("status" -> "OK", "program" -> f.program,"expressionType"-> stringBuilder.toString()))
            } catch {
              case te : ThrowableAnalysisError => Ok(Json.obj("status" -> "AnalysisKO", "issue" -> analysisErrorToUi(te.analysisError)))
              case e: Throwable =>
                print("error")
                print(e)
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }

          case Left(error) => Ok(Json.obj("status" -> "AnalysisKO", "issue" -> parseErrorToUi(error)))
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
        ObSecGParser(f.program) match {
          case Right(term) =>
            try {
              var modelTerm = ObSecGIdentifierResolver(term)
              val result = InterpreterG.run(modelTerm)
              Ok(Json.obj("status" -> "OK",
                "program" -> f.program,
                "result"-> result.toString
              ))
            }
            catch {
              case e: Throwable =>
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }
          //throw new  Exception(confs.last.toString+ " "+error.toString)
          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> "Parser error"))
        }

      }
    )
  }
}
