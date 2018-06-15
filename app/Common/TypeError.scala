package Common

import scala.util.parsing.input.{NoPosition, Position, Positional}

trait OffsetPositional extends Positional{
  var offset:Int = -1
  def setOffSet(n:Int): this.type ={
    if(offset == -1)offset = n
    this
  }

  var endPos:Position = NoPosition
  def setEndPos(pos:Position):this.type ={
    if(endPos eq NoPosition) endPos = pos
    this
  }
}
trait AstNode extends OffsetPositional
object NoAstNode extends AstNode

trait ErrorCodesEnum
trait ErrorCode{
  def code: ErrorCodesEnum
  def message:String
}


class AnalysisError(var node: AstNode, val errorCode: ErrorCode,val parameters: List[String] = List()){
  override def toString: String = errorCode.message.format(parameters:_*)

  def setNode(n:AstNode):AnalysisError ={
    if(node eq NoAstNode) node = n
    this
  }
}

trait ThrowableAnalysisError extends Error{
  def analysisError: AnalysisError
}

case class TypeError(str: String) extends Error

object ImplementationError extends ErrorCodesEnum
object SecTypeIsNotWellFormed extends ErrorCodesEnum
object VariableAlreadyDefinedInScope extends ErrorCodesEnum

case class CommonErrorCode(code: ErrorCodesEnum,message:String) extends ErrorCode

object CommonErrorCodes{
  val secTypeIsNotWellFormed: CommonErrorCode =
    CommonErrorCode(SecTypeIsNotWellFormed,
      "Security type: %s is not well-formed : %s")

  val variableAlreadyDefinedInScope: CommonErrorCode =
    CommonErrorCode(VariableAlreadyDefinedInScope,
      "Variable %s is already defined in its scope")

  val implementationError: CommonErrorCode =
    CommonErrorCode(ImplementationError,
      "Implementation error: %s")
}

case class CommonError(analysisError: AnalysisError) extends ThrowableAnalysisError

object CommonError{
  def implementationError(node:AstNode,message:String):CommonError =
    commonError(node,CommonErrorCodes.implementationError,List(message))

  def variableAlreadyDefined(str: String): CommonError =
    commonError(NoAstNode,CommonErrorCodes.variableAlreadyDefinedInScope,List(str))

  def secTypeIsNotWellFormed(theType:String, details:String): CommonError =
    commonError(NoAstNode,CommonErrorCodes.secTypeIsNotWellFormed,List(theType, details))

  private def commonError(node:AstNode,errorCode: ErrorCode, parameters: List[String]=List()) =
    CommonError(new AnalysisError(node,errorCode,parameters))
}

class StuckError(m: String = "") extends Error {
}
