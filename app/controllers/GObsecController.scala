package controllers

import java.nio.file.Paths
import javax.inject._

import Common.{AstNode, ParserError}
import ObSecG.Ast.ObSecGAstExprNode
import ObSecG.Parsing._
import ObSecG.Runtime.InterpreterG
import ObSecG.Static.TypeCheckerG
import models._


@Singleton
class GObsecController @Inject()(configuration: play.api.Configuration) extends BaseLanguageController(configuration) {

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
}
