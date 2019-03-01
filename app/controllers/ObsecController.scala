package controllers

import java.nio.file.Paths
import javax.inject._

import Common.{AstNode, ParserError}
import ObSec.Ast.ObSecExpr
import ObSec.Parsing._
import ObSec.Runtime._
import ObSec.Static._
import models._

@Singleton
class ObsecController @Inject()(configuration: play.api.Configuration) extends BaseLanguageController (configuration){

  override protected def getExamples:List[Example] = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    val exampleDir = configuration.underlying.getString("languagepad.obsec.exampleDirectory")
    val exampleDirFullPath = Paths.get(currentDirectory, exampleDir)
    val exampleHelper = new ExampleHelper(exampleDirFullPath.toString,".obsec")
    exampleHelper.examples()
  }
  override protected def getSyntax: List[SyntaxModel] = List()

  override protected def typeOf(node: AstNode): String = TypeChecker(node.asInstanceOf[ObSecExpr]).toString

  override protected def run(node: AstNode): String = Interpreter.run(node.asInstanceOf[ObSecExpr]).toString

  override protected def parse(p: String): Either[ParserError, AstNode] = ObSecParser(p)
}
