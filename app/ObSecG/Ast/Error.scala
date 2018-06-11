package ObSecG.Ast

sealed trait ErrorCodesEnum
object DuplicatedMethodInObjectType extends ErrorCodesEnum
object DuplicatedMethodInObject extends ErrorCodesEnum
object InvalidTypeForPrivateFacet extends  ErrorCodesEnum
object VariableAlreadyDefined extends ErrorCodesEnum
object TypeIsNotDefined extends ErrorCodesEnum
object VariableIsNotDefined extends ErrorCodesEnum

object ResolverErrorCodes{
  def duplicatedMethodInObjectType =
    ErrorCodeG(DuplicatedMethodInObjectType,
      "Duplicated method in object type")
  def duplicatedMethodInObject =
    ErrorCodeG(DuplicatedMethodInObject,
      "Duplicated method in object ")

  def invalidTypeForPrivateFacet =
    ErrorCodeG(InvalidTypeForPrivateFacet,
      "Invalid type for private facet. Private facet type just support: " +
        "object types, self type variables and builtin types")

  def variableAlreadyDefined =
    ErrorCodeG(VariableAlreadyDefined,
      "The variable %s is already defined in its scope")

  def typeIsNotDefined =
    ErrorCodeG(TypeIsNotDefined,
      "Type %s is not defined")

  def variableIsNotDefined =
    ErrorCodeG(VariableIsNotDefined,
      "Variable %s is not defined")
}

case class ErrorCodeG(code: ErrorCodesEnum, message: String)

class AnalysisError(val node: ObSecGAstNode, val errorCode: ErrorCodeG,val parameters: List[String] = List()){
  override def toString: String = errorCode.message.format(parameters)
}

case class ResolverError(analysisError: AnalysisError) extends Error
object ResolverError{
  def variableNotDefined(expression: ObSecGAstExprNode,variableName:String): ResolverError =
    resolverError(expression,ResolverErrorCodes.variableIsNotDefined,List(variableName))


  private def resolverError(node:ObSecGAstNode,errorCode: ErrorCodeG, parameters: List[String]=List())=
    ResolverError(new AnalysisError(node,errorCode,parameters))

  def typeIsNotDefined(typeAnnotation: TypeAnnotation, typeName: String): ResolverError =
    resolverError(typeAnnotation,ResolverErrorCodes.typeIsNotDefined,List(typeName))

  def variableAlreadyDefined(labelVar: ObSecGAstNode, variableName: String): ResolverError=
    resolverError(labelVar,ResolverErrorCodes.variableAlreadyDefined,List(variableName))

  def invalidTypeForPrivateFacet(annotatedFacetedType: AnnotatedFacetedType): ResolverError =
    resolverError(annotatedFacetedType,ResolverErrorCodes.invalidTypeForPrivateFacet)

  def duplicatedMethodInObjectType(node:ObSecGAstNode): ResolverError = {
    resolverError(node,ResolverErrorCodes.duplicatedMethodInObjectType)
  }

  def duplicatedMethodInObject(expression: ObSecGAstExprNode): ResolverError =
    resolverError(expression,ResolverErrorCodes.duplicatedMethodInObject)
}

//Analysis Error for type errors
object SameTypeForIfBranches extends ErrorCodesEnum
object IfConditionExpectABoolean extends ErrorCodesEnum
object ReturnTypeError extends ErrorCodesEnum
object MethodNotFound extends ErrorCodesEnum
object SubTypingError extends ErrorCodesEnum
object BadActualLabelArgument extends ErrorCodesEnum
object ActualTypeParametersMustMatchFormalTypeParameterAmount extends ErrorCodesEnum

object TypeCheckerErrorCodes{
  def sameTypeForIfBranches: ErrorCodeG =
    ErrorCodeG(SameTypeForIfBranches,
    "Both branches of an if expression must have the same type")

  def ifConditionExpectABoolean: ErrorCodeG =
    ErrorCodeG(IfConditionExpectABoolean,
      "The 'if' condition expected a boolean")

  def returnTypeError: ErrorCodeG =
    ErrorCodeG(ReturnTypeError,
      "Definition of method '%s': " +
        s"the return type in the implementation (%s) is not subtype of " +
        s"the return type in the signature (%s)")

  def methodNotFound: ErrorCodeG =
    ErrorCodeG(MethodNotFound,
      "Method %s not found")

  def subTypingError: ErrorCodeG =
    ErrorCodeG(SubTypingError,
      """Invocation of %s: Type %s
             (of actual argument) is not subtyping of %s""")
  def badActualLabelArgument: ErrorCodeG =
    ErrorCodeG(BadActualLabelArgument,
      "Invocation of %s : Actual type for generic" +
        s" type variable %s does not satisfy subtyping" +
        s" constraint")

  def actualTypeParametersMustMatchFormalTypeParameterAmount: ErrorCodeG =
    ErrorCodeG(ActualTypeParametersMustMatchFormalTypeParameterAmount,
      "Method '%s' : Actual types amount must" +
        s" match the formal type variable amount")
}

case class TypeErrorG(analysisError: AnalysisError) extends Error
object TypeErrorG{
  def sameTypeForIfBranches(node: ObSecGAstNode): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.sameTypeForIfBranches)

  def ifConditionExpectABoolean(node: ObSecGAstNode): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.ifConditionExpectABoolean)

  def returnTypeError(node:ObSecGAstNode, method: String, returnedType: String, expectedType: String): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.returnTypeError,List(method,returnedType,expectedType))

  def methodNotFound(node:ObSecGAstNode, method: String): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.methodNotFound,List(method))

  def subTypingError(node:ObSecGAstNode,method: String, actualType: STypeG, expectedType: STypeG): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.subTypingError,List(method,actualType.toString,expectedType.toString))

  def badActualLabelArgument(node:ObSecGAstNode, method: String, typeVar: String): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.badActualLabelArgument,List(method,typeVar))

  def actualTypeParametersMustMatchFormalTypeParameterAmount(node:ObSecGAstNode, method: String): TypeErrorG =
    typeError(node,TypeCheckerErrorCodes.actualTypeParametersMustMatchFormalTypeParameterAmount,List(method))

  private def typeError(node: ObSecGAstNode, errorCode: ErrorCodeG, parameters:List[String]=List())=
    TypeErrorG(new AnalysisError(node,errorCode,parameters))
}




