package controllers

import java.nio.file.Paths
import javax.inject._

import Common.{AstNode, ParserError}
import ObSec.Ast.ObSecExpr
import ObSec.Parsing._
import ObSec.Runtime._
import ObSec.Static._
import models._
import play.{Application, Environment}

@Singleton
class ObsecController @Inject()(configuration: play.api.Configuration,application:Environment) extends BaseLanguageController (configuration,application){

  override protected def getExamples:List[Example] = {
    examplesFromConfiguration("languagepad.obsec.exampleDirectory",".obsec")
  }
  override def getSyntax: SyntaxModel ={
    syntaxFromConfiguration("languagepad.obsec.syntaxFile")
  }

  override protected def typeOf(node: AstNode): String = TypeChecker(node.asInstanceOf[ObSecExpr]).toString

  override protected def run(node: AstNode): String = Interpreter.run(node.asInstanceOf[ObSecExpr]).toString

  override protected def parse(p: String): Either[ParserError, AstNode] = ObSecParser(p)
}
