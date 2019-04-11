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
import play.{Application, Environment}

import scala.collection.mutable


@Singleton
class GObsecController @Inject()(configuration: play.api.Configuration,application:Environment) extends BaseLanguageController(configuration,application) {

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

}









