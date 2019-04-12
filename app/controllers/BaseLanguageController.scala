package controllers

import java.io.File
import java.nio.file.Paths

import play.Environment
import play.Play
import Common.{AstNode, ParserError, ThrowableAnalysisError}
import models._
import play.api.libs.json.{Json, _}
import play.api.mvc._

abstract class BaseLanguageController (configuration: play.api.Configuration,environment:Environment) extends Controller {

  implicit val fReads = Json.reads[Program]
  implicit val errorPositionFormat = Json.format[UIErrorPosition]
  implicit val analysisErrorFormat = Json.format[UIAnalysisError]
  implicit val exampleFormat = Json.format[Example]

  implicit val systemItemFormat = Json.format[SyntaxItem]
  implicit val productionFormat = Json.format[SyntaxProduction]
  implicit val syntaxFormat = Json.format[SyntaxModel]


  def examples = Action { implicit request =>
    val l = getExamples
    Ok(Json.obj("status" -> "OK", "examples" -> l))
  }
  def syntax= Action { implicit request =>
    val l = getSyntax
    Ok(Json.obj("status" -> "OK", "syntax" -> l))
  }

  def typecheck = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[Program]
    fResult.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        parse(f.program) match {
          case Right(term) =>
            try {
              val typeRep = typeOf(term)
              Ok(Json.obj("status" -> "OK", "program" -> f.program,"expressionType"-> typeRep))
            } catch {
              case te : ThrowableAnalysisError =>
                Ok(Json.obj("status" -> "AnalysisKO", "issue" -> CommonModel.analysisErrorToUi(te.analysisError)))
              case e: Throwable =>
                print("error")
                print(e)
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }
          case Left(error) => Ok(Json.obj("status" -> "AnalysisKO", "issue" -> CommonModel.parseErrorToUi(error)))
        }
      }
    )
  }

  def reduce = Action(BodyParsers.parse.json) { implicit request =>
    val fResult = request.body.validate[Program]
    fResult.fold(
      errors => {
        print("Bad request:")
        println(fResult)
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      f => {
        parse(f.program) match {
          case Right(term) =>
            try {
              val result = run(term)
              Ok(Json.obj("status" -> "OK",
                "program" -> f.program,
                "result"-> result.toString
              ))
            }
            catch {
              case e: Throwable =>
                Ok(Json.obj("status" -> "KO", "error" -> e.getMessage))
            }
          case Left(error) => Ok(Json.obj("status" -> "KO", "error" -> "Parser error"))
        }
      }
    )
  }
  protected def getExamples : List[Example]
  protected def getSyntax: SyntaxModel
  protected def parse(p:String): Either[ParserError, AstNode]
  protected def typeOf(node:AstNode): String
  protected def run(node:AstNode): String


  protected def examplesFromConfiguration(exampleDirectoryConfigurationKey:String,
                                          exampleExtensions: String):List[Example] = {
    val currentDirectory = PathHelper.getActualRootFolder(environment)
    val exampleDir = configuration.underlying.getString(exampleDirectoryConfigurationKey)
    val exampleDirFullPath = Paths.get(currentDirectory, exampleDir)
    val exampleHelper = new ExampleHelper(exampleDirFullPath.toString, exampleExtensions)
    exampleHelper.examples()
  }

  protected def syntaxFromConfiguration(syntaxFileConfigurationKey:String):SyntaxModel = {
    val currentDirectory = PathHelper.getActualRootFolder(environment)
    val syntaxFile = configuration.underlying.getString(syntaxFileConfigurationKey)
    val syntaxFileFullPath = Paths.get(currentDirectory, syntaxFile)
    val exampleHelper = new SyntaxHelper(syntaxFileFullPath.toString)
    exampleHelper.syntax()
  }
}
object PathHelper{
  /**
    * Hack to get the application root folder. For some reason that I haven't understood
    * yet, Environment.rootPath() keeps returning the running path, which (at least in Windows)
    * is not necesarily the same that the application root folder.
    * @param environment
    * @return
    */
  def getActualRootFolder(environment: Environment):String={
    val resourcePath = environment.classLoader().getResource("public").getPath
    //println("'Public' resource uri" + resourcePath)
    //remove file:/
    val pathWithoutProtocol = resourcePath.substring("file:/".length)
    //println("Resource uri no protocol" + pathWithoutProtocol)
    val pathWithoutResource = pathWithoutProtocol.substring(0,pathWithoutProtocol.length - "public".length - 2)
    //println("Lib folder" + pathWithoutResource)

    val rootFolder = new File(pathWithoutResource).getParentFile.getParentFile
    //println(s"Root Folder: $rootFolder")
    rootFolder.getAbsolutePath
  }
}
