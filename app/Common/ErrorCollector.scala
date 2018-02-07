package Common

/**
  * Created by racruz on 04-04-2017.
  */
class ErrorCollector {
  private var  _errors = List[String]()
  def report(x:String): Unit = {_errors = _errors ++ List(x)}
  def errors: String = if(_errors.isEmpty)"" else _errors.head+ _errors.drop(1).foldLeft("")((acc, x)=> acc+" . "+"x")
}
