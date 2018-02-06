package Common

/**
  * Created by racruz on 04-04-2017.
  */
class ErrorCollector {
  var _errors = List[String]()
  def report(x:String) = {_errors = (_errors ++ List(x))}
  def errors = if(_errors.size==0)"" else _errors(0)+ _errors.drop(1).foldLeft("")((acc,x)=> acc+" . "+"x")
}
