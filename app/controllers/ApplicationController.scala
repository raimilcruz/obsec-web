package controllers

import javax.inject._

import play.api.libs.json.{Json, Writes}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import ObSec.Parsing.ObSecParser
import ObSec.Runtime.Interpreter
import ObSec.Static.TypeChecker
import ObSecG.Static.TypeCheckerG
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG

import scala.pickling.Defaults._
import scala.pickling.json._

@Singleton
class ApplicationController extends Controller {

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
              val aType = TypeCheckerG(term)
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
              val result = InterpreterG.run(term)
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
