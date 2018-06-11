package controllers

import javax.inject._

import ObSecG.Parsing.{ObSecGParser, ObSecGIdentifierResolver}
import ObSecG.Runtime.InterpreterG
import ObSecG.Static.TypeCheckerG
import play.api.libs.json.{Json, _}
import play.api.mvc._

@Singleton
class ObSecGController extends Controller {

  case class Program(program: String)

  implicit val fReads = Json.reads[Program]

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
              Ok(Json.obj("status" -> "OK", "program" -> f.program,"expressionType"-> aType.toString))
            } catch {
              case te : Common.TypeError => Ok(Json.obj("status" -> "KO", "error" -> te.str))
              case e: Throwable =>
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }

          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> s"Parser error. ${error.msg}"))
        }

      }
    )
  }

  def reduce = Action(BodyParsers.parse.json) { implicit request =>
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
