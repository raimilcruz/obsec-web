package controllers

import java.nio.file.Paths
import javax.inject._

import Common.judgment._
import Common.{AstNode, ParserError, ThrowableAnalysisError}
import ObSecG.Ast.ObSecGAstExprNode
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG
import ObSecG.Static.TypeCheckerG
import models._
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers}
import models.JsonFormatters.JudgmentJsonWriters._
import models.JsonFormatters.JudgmentJsonReaders._
import scala.collection.mutable


@Singleton
class GObsecController @Inject()(configuration: play.api.Configuration) extends BaseLanguageController(configuration) {
  implicit val jrEqReads = Json.reads[JudgmentRequestLessEq]

  //implicit val emptyJudgmentContentFormatter = Json.format[Nat]
  //implicit val lessEqJudgmentFormatter = Json.format[LessEqJudgmentGoal]
  //implicit val judgmentPremiseFormatter = Json.format[JudgmentPremiseImpl]

  override protected def getExamples : List[Example] = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    val exampleDir = configuration.underlying.getString("languagepad.gobsec.exampleDirectory")
    val exampleDirFullPath = Paths.get(currentDirectory, exampleDir)
    val exampleHelper = new ExampleHelper(exampleDirFullPath.toString,".gobsec")
    exampleHelper.examples()
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


  def judgmentAdapters : Map[String,JudgmentAdapter] = Map(
    "less-eq" -> new LessEqJudgmentAdapter,
    "eq" -> new EqJudgmentAdapter
  )

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

        /*

        if (f.n1 <= f.n2)
          Ok(Json.obj("status" -> "OK"))
        else
          Ok(Json.obj("status" -> "AnalysisKO","error" ->"n1 is bigger"))*/
      }
    )
  }

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
}
case class JudgmentRequestApi(key:String,stepId:String,context:JudgmentContext,goal:JudgmentGoal)
case class JudgmentPremiseApi(key:String,premise:JudgmentPremise){
  def toJudgmentPremise:JudgmentPremise= premise
}
object JudgmentPremiseApi{
  def from(key:String,premise:JudgmentPremise):JudgmentPremiseApi= JudgmentPremiseApi(key,premise)
}
case class JudgmentStepApi(key:String,stepId:String,step:JudgmentStep)

/*
  """
      {
        "key": "less-eq"
        context: null,
        goal: {
          n1: 1
          n2: 2
        }
      }
    """
 */

//Entrance Door to judgment machinery for the LessEq judgment
case class JudgmentRequestLessEq(n1:String,n2:String)







