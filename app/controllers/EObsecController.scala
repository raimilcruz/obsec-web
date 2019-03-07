package controllers

import javax.inject._

import Common.{AstNode, ParserError}
import ObSecG.Ast.ObSecGAstExprNode
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG
import ObSecG.Static.TypeCheckerG
import models._


@Singleton
class EObsecController @Inject()(configuration: play.api.Configuration) extends BaseLanguageController(configuration) {

  override protected def getExamples : List[Example] = {
    examplesFromConfiguration("languagepad.eobsec.exampleDirectory",".eobsec")
  }


  override def getSyntax: SyntaxModel ={
    syntaxFromConfiguration("languagepad.eobsec.syntaxFile")
  }

  override protected def typeOf(term:AstNode):String= ???
  override protected def run(term:AstNode):String= ???
  override protected def parse(p: String): Either[ParserError, AstNode] = ???

}









