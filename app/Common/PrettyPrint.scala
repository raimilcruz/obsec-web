package Common

trait PrettyPrint{
  def prettyPrint():String
}
//implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
class BooleanPrettyPrint(bool:Boolean)