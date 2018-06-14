trait AnalysisError{

}
trait RuntimeError{
}


trait LanguageService {
  def typeCheck(program: String): Either[String,AnalysisError]
  def eval(program:String):Either[String,RuntimeError]
}
