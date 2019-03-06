package controllers

import java.nio.file.Paths
import javax.inject._

import Common.judgment._
import Common.judgment.examples._
import Common.judgment.examples.lambdacalculus.{TypingEnvironment, TypingGoal, TypingJudgmentAdapter, TypingPremise}
import Common.{AstNode, ParserError, ThrowableAnalysisError}
import ObSecG.Ast.ObSecGAstExprNode
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG
import ObSecG.Static.TypeCheckerG
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers}
import models._
import models.JsonFormatters.JudgmentJsonWriters._
import models.JsonFormatters.JudgmentJsonReaders._
import models.JsonFormatters.Typing.LambdaCalculusParser
import models.judgment.JudgmentPremiseApi
import models.judgment.GlobalAdapters._

import scala.collection.mutable


@Singleton
class GObsecController @Inject()(configuration: play.api.Configuration) extends BaseLanguageController(configuration) {
  //Entrance Door to judgment machinery for the LessEq judgment
  case class JudgmentRequestLessEq(n1:String,n2:String)
  case class JudgmentRequestTyping(context:String,expr: String)

  implicit val jrEqReads = Json.reads[JudgmentRequestLessEq]
  implicit val jrTypingReads = Json.reads[JudgmentRequestTyping]


  override protected def getExamples : List[Example] = {
    examplesFromConfiguration("languagepad.gobsec.exampleDirectory",".gobsec")
  }


  override def getSyntax: SyntaxModel ={
    syntaxFromConfiguration("languagepad.gobsec.syntaxFile")
  }

  override protected def typeOf(term:AstNode):String={
    var modelTerm = ObSecGIdentifierResolver(term.asInstanceOf[ObSecGAstExprNode])
    val aType = TypeCheckerG(modelTerm)
    val stringBuilder = new StringBuilder
    aType.prettyPrint(stringBuilder)
    stringBuilder.toString
  }
  override protected def run(term:AstNode):String={
    var modelTerm = ObSecGIdentifierResolver(term.asInstanceOf[ObSecGAstExprNode])
    InterpreterG.run(modelTerm).toString
  }

  override protected def parse(p: String): Either[ParserError, AstNode] = ObSecGParser(p)

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









