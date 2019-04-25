package Common

sealed trait SubtypingResult {
  def &&(function: => SubtypingResult) : SubtypingResult
}
object SubtypingSuccess extends SubtypingResult {
  override def &&(function: => SubtypingResult): SubtypingResult = function
}
case class SubtypingFail[T](left: T, righ:T) extends SubtypingResult {
  override def &&(function: => SubtypingResult): SubtypingResult = this

  var message:String = ""
  def setMessage(s:String):SubtypingFail[T]={
    message = s
    this
  }
}
