package Common

case class DefaultErrorCode(code: ErrorCodesEnum, message: String) extends ErrorCode

object GeneralError extends ErrorCodesEnum
object DuplicatedMethodInObjectType extends ErrorCodesEnum
object DuplicatedMethodInObject extends ErrorCodesEnum
object InvalidTypeForPrivateFacet extends  ErrorCodesEnum
object VariableAlreadyDefined extends ErrorCodesEnum
object TypeIsNotDefined extends ErrorCodesEnum
object VariableIsNotDefined extends ErrorCodesEnum

object ResolverErrorCodes{
  def generalError =
    DefaultErrorCode(GeneralError,
    "%s")

  def duplicatedMethodInObjectType =
    DefaultErrorCode(DuplicatedMethodInObjectType,
      "Duplicated method in object type")
  def duplicatedMethodInObject =
    DefaultErrorCode(DuplicatedMethodInObject,
      "Duplicated method in object ")

  def invalidTypeForPrivateFacet =
    DefaultErrorCode(InvalidTypeForPrivateFacet,
      "Invalid type for private facet. Private facet type just support: " +
        "object types, self type variables and builtin types")

  def variableAlreadyDefined =
    DefaultErrorCode(VariableAlreadyDefined,
      "The variable %s is already defined in its scope")

  def typeIsNotDefined =
    DefaultErrorCode(TypeIsNotDefined,
      "Type %s is not defined")

  def variableIsNotDefined =
    DefaultErrorCode(VariableIsNotDefined,
      "Variable %s is not defined")

}

case class ResolverError(analysisError: AnalysisError) extends ThrowableAnalysisError
object ResolverError{
  def generalError(astNode:AstNode,errorMessage:String):ResolverError =
    resolverError(astNode,ResolverErrorCodes.variableIsNotDefined,List(errorMessage))

  def variableNotDefined(expression: AstNode,variableName:String): ResolverError =
    resolverError(expression,ResolverErrorCodes.variableIsNotDefined,List(variableName))


  private def resolverError(node:AstNode,errorCode: ErrorCode, parameters: List[String]=List())=
    ResolverError(new AnalysisError(node,errorCode,parameters))

  def typeIsNotDefined(typeAnnotation: AstNode, typeName: String): ResolverError =
    resolverError(typeAnnotation,ResolverErrorCodes.typeIsNotDefined,List(typeName))

  def variableAlreadyDefined(labelVar: AstNode, variableName: String): ResolverError=
    resolverError(labelVar,ResolverErrorCodes.variableAlreadyDefined,List(variableName))

  def invalidTypeForPrivateFacet(annotatedFacetedType: AstNode): ResolverError =
    ResolverError.resolverError(annotatedFacetedType,ResolverErrorCodes.invalidTypeForPrivateFacet)

  def duplicatedMethodInObjectType(node:AstNode): ResolverError = {
    resolverError(node,ResolverErrorCodes.duplicatedMethodInObjectType)
  }

  def duplicatedMethodInObject(expression: AstNode): ResolverError =
    resolverError(expression,ResolverErrorCodes.duplicatedMethodInObject)
}