package Common.judgment

/*
Receives a judgment request give option to execute the judgment
 */
trait JudgmentAdapter{
  def key:String
  def step(request: JudgmentRequest): JudgmentStep
}

trait PredicateAdapter{
  def key:String
  def eval(args: List[MetaExpr]): Either[Boolean,String]
}
trait FunctionAdapter{
  def key:String
  def eval(args: List[MetaExpr]): Either[MetaExpr,String]
}
sealed trait FunctionResult[+T]{
  def get:T
}
case class FunctionSuccess[T](result:T) extends FunctionResult[T] {
  override def get: T = result
}
case class FunctionFail(failMessage:String) extends  FunctionResult[Nothing] {
  override def get: Nothing = throw new NoSuchElementException("FunctionFail.get")
}


