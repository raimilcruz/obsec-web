package Common

import scala.util.parsing.input.Position

case class ParserError(msg: String, pos:Position, endPos:Position, offset:Int)
