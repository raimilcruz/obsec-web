package Common


case class TypeError(str: String) extends Error

abstract class ErrorCode(val errorName:String,val message:String){}

class CommonErrorCode(errorName: String,message:String)
  extends ErrorCode(errorName,message)

object CommonErrorCode{
  val secTypeIsNotWellFormed: CommonErrorCode =
    new CommonErrorCode("secTypeIsNotWellFormed",
      "Security type: %s is not well-formed : %s")
}

class CommonTypeError(errorCode: ErrorCode,
                           arguments:List[String])
  extends TypeError(errorCode.message.format(arguments:_*)) {

  override def toString: String = this.errorCode.message.format(arguments:_*)
}

object CommonTypeError{
  def secTypeIsNotWellFormed(theType:String, details:String)=
    new CommonTypeError(CommonErrorCode.secTypeIsNotWellFormed,List(theType, details))
}

class StuckError(m: String = "") extends Error {
}
