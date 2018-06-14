package Common

trait PrettyPrint{
  def prettyPrint(buffer:StringBuilder):Unit
  def newLine(emptySpacesAtFront:Int):String =
    (1 until emptySpacesAtFront).foldLeft("")((acc,e)=> acc + " ") + "\n"
}
//implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
class BooleanPrettyPrint(bool:Boolean)