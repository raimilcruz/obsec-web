package models

import Common.{AnalysisError, ParserError}

case class Program(program: String)
case class UIAnalysisError(kind:String, message: String,position: UIErrorPosition)
case class UIErrorPosition(line: Int, columnStart: Int, lineEnd: Int, columnEnd: Int)

object CommonModel {
  private def normalizeNumber(n:Int):Int={
    if(n>0) n-1
    else 0
  }

  def analysisErrorToUi(analysis:AnalysisError,kind:String = "error"):UIAnalysisError ={
    UIAnalysisError(
      kind,
      analysis.toString,
      UIErrorPosition(
        normalizeNumber(analysis.node.pos.line),
        normalizeNumber(analysis.node.pos.column),
        normalizeNumber(analysis.node.endPos.line),
        normalizeNumber(analysis.node.endPos.column)))
  }
  def parseErrorToUi(error:ParserError):UIAnalysisError ={
    UIAnalysisError(
      "parser error",
      error.msg,
      UIErrorPosition(
        normalizeNumber(error.pos.line),
        normalizeNumber(error.pos.column),
        normalizeNumber(error.endPos.line),
        normalizeNumber(error.endPos.column)))
  }
}
