package controllers

import javax.inject._

import Common.{AstNode, ParserError}
import ObSecE.Ast.EObSecNode
import ObSecE.Parsing._
import ObSecE.Runtime._
import ObSecE.Static.EObSecTypeChecker
import models._
import play.{Application, Environment}


@Singleton
class EObsecController @Inject()(configuration: play.api.Configuration,application:Environment) extends BaseLanguageController(configuration,application) {

  override protected def getExamples : List[Example] = {
    examplesFromConfiguration("languagepad.eobsec.exampleDirectory",".eobsec")
  }


  override def getSyntax: SyntaxModel ={
    syntaxFromConfiguration("languagepad.eobsec.syntaxFile")
  }

  override protected def typeOf(term:AstNode):String= {
    var modelTerm = EOBSecIdentifierResolver(term.asInstanceOf[EObSecNode])
    val aType = EObSecTypeChecker(modelTerm)
    val stringBuilder = new StringBuilder
    aType.prettyPrint(stringBuilder)
    stringBuilder.toString
  }
  override protected def run(term:AstNode):String= {
    val modelTerm = EOBSecIdentifierResolver(term.asInstanceOf[EObSecNode])
    EObSecInterpreter.run(modelTerm).toString
  }
  override protected def parse(p: String): Either[ParserError, AstNode] = EObSecParser(p)

}









