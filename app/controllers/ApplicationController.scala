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
        ObSecParser(f.program) match {
          case Right(term) =>
            try {
              val aType = TypeChecker(term)
              Ok(Json.obj("status" -> "OK", "program" -> f.program,"expressionType"-> aType.toString))
            } catch {
              case te : ObSec.Static.TypeError => Ok(Json.obj("status" -> "KO", "error" -> te.str))
              case e: Throwable =>
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }

          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> s"Parse error: ${error.msg}"))
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
        ObSecParser(f.program) match {
          case Right(term) =>
            try {
              val result = Interpreter.run(term)
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
          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> "Parse error"))
        }

      }
    )
  }
}
