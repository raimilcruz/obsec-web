package ObSecE.Ast

import Common._

//Analysis Error for type errors
object ActualTypeParametersMustMatchFormalTypeParameterAmount extends ErrorCodesEnum

object BadStringListLabel extends ErrorCodesEnum

case class ErrorCodeE(code: ErrorCodesEnum, message: String) extends ErrorCode
object TypeCheckerErrorCodes{
}


object InvalidTypeForImplementationType extends  ErrorCodesEnum
object EObSecResolverErrorCodes{
  def invalidTypeForImplementationType =
    DefaultErrorCode(InvalidTypeForImplementationType,
      "Invalid type for implementation type. Implementation types can be : " +
        "object types, self type variables and builtin types")
}
object EObSecResolverError{
  private def resolverError(node:AstNode,errorCode: ErrorCode, parameters: List[String]=List())=
    ResolverError(new AnalysisError(node,errorCode,parameters))

  def invalidTypeForImplementationType(typeAnnotation: AstNode): ResolverError =
    resolverError(typeAnnotation,EObSecResolverErrorCodes.invalidTypeForImplementationType,List())

}



object InvalidExitentialFacet extends  ErrorCodesEnum
object EObSecWellFormednessErrorCodes{
  def invalidExitentialFacet =
    DefaultErrorCode(InvalidExitentialFacet,
      "Invalid existential faceted type: %s")
}
object EObSecWellFormednessError{
  private def resolverError(node:AstNode,errorCode: ErrorCode, parameters: List[String]=List())=
    ResolverError(new AnalysisError(node,errorCode,parameters))

  def invalidExistentialFacet(node: AstNode, message:String): ResolverError =
    resolverError(node,EObSecWellFormednessErrorCodes.invalidExitentialFacet,List(message))
}

case class TypeErrorE(analysisError: AnalysisError) extends ThrowableAnalysisError
object TypeErrorE{

  private def typeError(node: AstNode, errorCode: ErrorCodeE, parameters:List[String]=List())=
    TypeErrorE(new AnalysisError(node,errorCode,parameters))
}




