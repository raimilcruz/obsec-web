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

  var source : CharSequence = ""
  def setSource(s :CharSequence): this.type = {
    if(source eq "") source =s
    this
  }
}


trait AstNode extends OffsetPositional{
  def children : List[AstNode]
}

object NoAstNode extends AstNode {
  override def children: List[AstNode] = List()
}
trait TypeAnnotation extends AstNode