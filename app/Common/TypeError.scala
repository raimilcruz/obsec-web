package Common


trait ErrorCodesEnum
trait ErrorCode{
  def code: ErrorCodesEnum
  def message:String
}


class AnalysisError(var node: AstNode, val errorCode: ErrorCode,val parameters: List[String] = List()){
  override def toString: String = errorCode.message.format(parameters:_*)

  def setNode(n:AstNode):AnalysisError ={
    if(node eq NoAstNode) node = n
    this
  }
}

trait ThrowableAnalysisError extends Error{
  def analysisError: AnalysisError
}

object ImplementationError extends ErrorCodesEnum
object SecTypeIsNotWellFormed extends ErrorCodesEnum
object VariableAlreadyDefinedInScope extends ErrorCodesEnum
object GenericErrorCode extends ErrorCodesEnum

//obsec calculi common errors
object SameTypeForIfBranches extends ErrorCodesEnum
object ActualArgumentsSizeError extends ErrorCodesEnum
object ReturnTypeError extends ErrorCodesEnum
object IfConditionExpectABoolean extends ErrorCodesEnum
object MethodNotFound extends ErrorCodesEnum
object SubTypingError extends ErrorCodesEnum
object MissingMethodDefinition extends ErrorCodesEnum
object NamedTypeIsNotWellFormed extends ErrorCodesEnum
object FacetedWiseSubtypingError extends ErrorCodesEnum


case class CommonErrorCode(code: ErrorCodesEnum,message:String) extends ErrorCode

object CommonErrorCodes{
  def genericError: CommonErrorCode =
    CommonErrorCode(GenericErrorCode,"%s")

  val secTypeIsNotWellFormed: CommonErrorCode =
    CommonErrorCode(SecTypeIsNotWellFormed,
      "Security type: %s is not well-formed : %s")

  val variableAlreadyDefinedInScope: CommonErrorCode =
    CommonErrorCode(VariableAlreadyDefinedInScope,
      "Variable %s is already defined in its scope")

  val implementationError: CommonErrorCode =
    CommonErrorCode(ImplementationError,
      "Implementation error: %s")

  def sameTypeForIfBranches: CommonErrorCode =
    CommonErrorCode(SameTypeForIfBranches,
      "Both branches of an if expression must have the same type. Branch types: %s and %s")

  def actualArgumentsSizeError: CommonErrorCode =
    CommonErrorCode(ActualArgumentsSizeError,
      "Method '%s' : Actual arguments amount must" +
        s" match the formal arguments amount")

  def returnTypeError: CommonErrorCode =
    CommonErrorCode(ReturnTypeError,
      "Definition of method '%s': " +
        s"the return type in the implementation (%s) is not subtype of " +
        s"the return type in the signature (%s)")

  def ifConditionExpectABoolean: CommonErrorCode =
    CommonErrorCode(IfConditionExpectABoolean,
      "The 'if' condition expected a boolean")

  def methodNotFound: CommonErrorCode =
    CommonErrorCode(MethodNotFound,
      "Method '%s' not found")

  def subTypingError: CommonErrorCode =
    CommonErrorCode(SubTypingError,
      """Invocation of %s: Type %s
             (of actual argument) is not subtyping of %s""")

  def missingMethodDefinition: CommonErrorCode =
    CommonErrorCode(MissingMethodDefinition,
      s"There must exist a method definition of each" +
        s" method signature. Missing method definition" +
        s" for: %s")

  def namedTypeIsNotWellFormed: CommonErrorCode =
    CommonErrorCode(NamedTypeIsNotWellFormed,
      "Type '%s' is not well formed. Details: %s")

  def facetedWiseSubtypingError: CommonErrorCode =
    CommonErrorCode(FacetedWiseSubtypingError,
      "The Private facet must be subtype of the public facet in security type %s")
}

case class CommonError(analysisError: AnalysisError) extends ThrowableAnalysisError

object CommonError{
  def genericError(astNode: AstNode, message: String): CommonError =
    commonError(astNode,CommonErrorCodes.genericError,List(message))

  def implementationError(node:AstNode,message:String):CommonError =
    commonError(node,CommonErrorCodes.implementationError,List(message))

  def variableAlreadyDefined(str: String): CommonError =
    commonError(NoAstNode,CommonErrorCodes.variableAlreadyDefinedInScope,List(str))

  def secTypeIsNotWellFormed(theType:String, details:String,astNode:AstNode= NoAstNode): CommonError =
    commonError(astNode,CommonErrorCodes.secTypeIsNotWellFormed,List(theType, details))

  private def commonError(node:AstNode,errorCode: ErrorCode, parameters: List[String]=List()) =
    CommonError(new AnalysisError(node,errorCode,parameters))

  def sameTypeForIfBranches(node: AstNode, typeThen:String,typeElse:String): CommonError =
    commonError(node,CommonErrorCodes.sameTypeForIfBranches,List(typeThen,typeElse))

  def actualArgumentsSizeError(node: AstNode,method:String): CommonError =
    commonError(node,CommonErrorCodes.actualArgumentsSizeError,List(method))

  def ifConditionExpectABoolean(node: AstNode): CommonError =
    commonError(node,CommonErrorCodes.ifConditionExpectABoolean)

  def returnTypeError(node:AstNode, method: String, returnedType: String, expectedType: String): CommonError =
    commonError(node,CommonErrorCodes.returnTypeError,List(method,returnedType,expectedType))

  def methodNotFound(node:AstNode, method: String): CommonError =
    commonError(node,CommonErrorCodes.methodNotFound,List(method))

  def subTypingError(node:AstNode,method: String, actualType: String, expectedType: String): CommonError =
    commonError(node,CommonErrorCodes.subTypingError,List(method,actualType.toString,expectedType.toString))

  def missingMethodDefinition(astNode: AstNode, methodDefNames: List[String]): CommonError =
    commonError(astNode,CommonErrorCodes.missingMethodDefinition,List(methodDefNames.mkString(", ")))

  def namedTypeIsNotWellFormed(node: AstNode,typeName:String,wellFormedError:String): CommonError =
    commonError(node,CommonErrorCodes.namedTypeIsNotWellFormed,List(typeName,wellFormedError))

  def facetedWiseSubtypingError(node: AstNode,typeRep:String): CommonError =
    commonError(node,CommonErrorCodes.facetedWiseSubtypingError,List(typeRep))

}

class StuckError(m: String = "") extends Error

