package EObSec.Parsing

import Common._
import ObSecE.Ast._

class IdentifierResolver {

  /**
    * This method performs the following tasks:
    * 1. Resolves all TypeIdentifier node to:
    *  a)LabelVar is the type variable references a label variable definition
    *  b)TypeVar is the type variable references a self variable or a type alias
    *
    * 2. Returns the ast model, compating several surface expression
    * 3. Checks trivial conditions:
    *   a)Repeated methods
    * @param expression The ast model
    * @return
    */
  def resolve(expression: AstNode):EObSecExpr =
    resolve(new Scope,new Scope,expression)

  def resolveType(typeAnnotation: TypeAnnotation):LabelE=
    resolveType(new Scope,typeAnnotation,labelPosisition = true)

  private def resolve(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                      valueIdentifier: Scope[Boolean],
                      expression: AstNode):EObSecExpr =
    resolveInternal(typeIdentifierScope,valueIdentifier,expression).setAstNode(expression)



  private def resolveInternal(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                              valueIdentifier: Scope[Boolean],
                              expression: AstNode):EObSecExpr =expression match{
    case VariableNode(n) =>
      if(valueIdentifier.contains(n))
        Var(n)
      else
        throw ResolverError.variableNotDefined(expression,n)
    case ObjectDefinitionNode(self,typeAnnotation,methods) =>
      if (methods.map(x => x.name).distinct.lengthCompare(methods.size) == 0) {
        var objectScope = new NestedScope(valueIdentifier)
        objectScope.add(self,true)
        val resolvedSelfType = resolveAnnotatedFacetedType(typeIdentifierScope, typeAnnotation)
        Obj(self,
          resolvedSelfType,
          methods.map(meth => {
            val methodLabelDefinitionScope = new NestedScope(typeIdentifierScope)
            var methodValueVariableScope = new NestedScope(objectScope)

            //add method to scope
            meth.args.elems.foreach(x=> liftError(methodValueVariableScope.add(x.name,true),x))

            resolveMethodDefinition(methodLabelDefinitionScope ,methodValueVariableScope, meth)
          })
        )
      }
      else{
        print(methods)
        throw ResolverError.duplicatedMethodInObject(methods.reverse.groupBy(identity).collect({case (x,List(_,_,_*)) => x}).head)
      }
    case MethodInvocationNode(e1,actualTypes,actualArguments,name)=>
      MethodInv(
        resolve(typeIdentifierScope,valueIdentifier,e1),
        NodeList(actualTypes.elems.map(at=> resolveType(typeIdentifierScope,at,labelPosisition = true))).setAstNode(actualTypes),
        NodeList(actualArguments.elems.map(aa => resolve(typeIdentifierScope,valueIdentifier,aa))).setAstNode(actualArguments),
        name.name
      ).setMethodNameNode(name)
    case BooleanLiteral(b) => BooleanExpr(b)
    case IntLiteral(n) => IntExpr(n)
    case StringLiteral(s) => StringExpr(s)
    case IfExpressionNode(c,e1,e2)=>
      IfExpr(resolve(typeIdentifierScope,valueIdentifier,c),
        resolve(typeIdentifierScope,valueIdentifier,e1),
        resolve(typeIdentifierScope,valueIdentifier,e2))
    case ListLiteral(label,elems)=>
      ListConstructorExpr(
        resolveType(typeIdentifierScope, label,labelPosisition = true),
        elems.map(e=>resolve(typeIdentifierScope,valueIdentifier,e)))
    case ConsListOperatorNode(e1,e2)=>
      ConsListExpr(resolve(typeIdentifierScope,valueIdentifier,e1),
        resolve(typeIdentifierScope,valueIdentifier,e2))
    case LetStarExpressionNode(declarations,body)=>
      val letTypeScope = new NestedScope(typeIdentifierScope)
      val letValueIdentifierScope = new NestedScope(valueIdentifier)
      LetStarExpr(declarations.map(d => {
        d match {
          case node: LocalDeclarationNode => letValueIdentifierScope.add(node.variable, true)
          case node: TypeAliasDeclarationNode =>
            val typeAliasDeclaration= TypeDeclarationPoint(resolveType(letTypeScope,node.objType,labelPosisition = true).asInstanceOf[ObjectType])
            letTypeScope.add(node.aliasName, typeAliasDeclaration)
          case node: DefTypeNode =>
            val defTypeDeclaration = TypeDeclarationPoint(null)
            letTypeScope.add(node.name, defTypeDeclaration)
            val objectType = ObjectType(node.name,node.methods.map(m=> resolveMethodDeclaration(letTypeScope,m)))
            defTypeDeclaration.definingType = objectType

        }
        resolveDeclaration(letTypeScope,valueIdentifier,d).setAstNode(d)
      }),
        resolve(letTypeScope,letValueIdentifierScope,body))
    case _ => throw new NotImplementedError("ObSecGIdentifierResolver.resolveInteral not implemented")
  }

  def liftError(action: => Unit, node: AstNode): Unit = {
    try{
      action
    }
    catch{
      case te: ThrowableAnalysisError =>
        te.analysisError.setNode(node)
        throw te
      case e:Throwable => throw e
    }
  }

  private def resolveDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                         valueIdentifier: Scope[Boolean],
                         declaration: DeclarationNode): Declaration= declaration match{
    case DefTypeNode(typeName,methods)=>
      TypeDefinition(typeName,methods.map(m=> resolveMethodDeclaration(typeIdentifierScope,m)))
    case TypeAliasDeclarationNode(typeAlias,objType)=>
      TypeAlias(typeAlias,resolveType(typeIdentifierScope,objType,labelPosisition = false).asInstanceOf[ObjectType])
    case LocalDeclarationNode(name,expr)=>
      LocalDeclaration(name,resolve(typeIdentifierScope,valueIdentifier,expr))
  }



  private def resolveMethodDefinition(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                      valueIdentifier: Scope[Boolean],
                                      md: MethodDefinitionNode):MethodDef ={
    MethodDef(md.name,md.args.elems.map(x=>x.name),resolve(typeIdentifierScope,valueIdentifier,md.mBody)).setAstNode(md)
  }

  private def resolveType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                          typeAnnotation: TypeAnnotation,
                          labelPosisition: Boolean):LabelE = (typeAnnotation match{

    case ObjectTypeNode(selfVar,methods) =>
      val objectTypeScope = new NestedScope(typeIdentifierScope)
      val selfDeclarationPoint  = TypeDeclarationPoint(null)
      objectTypeScope.add(selfVar,selfDeclarationPoint)
      val objectType = ObjectType(selfVar,methods.map(m=> resolveMethodDeclaration(objectTypeScope,m)))
      selfDeclarationPoint.definingType = objectType
      objectType
    case NoRecursiveObjectTypeNode(methods)=> resolveType(typeIdentifierScope,ObjectTypeNode("gen",methods),labelPosisition)
    case TypeIdentifier(n) =>
      val namedType = resolveBuiltinNamedTypes(n,labelPosisition)
      namedType match{
        case Left(t)=>t
        case Right(_) =>
          if(typeIdentifierScope.contains(n)) {
            val definitionPoint = typeIdentifierScope.lookup(n)
            definitionPoint match{
              case TypeDeclarationPoint(_) => TypeVar(n)
              case LabelDeclarationPoint => LabelVar(n)
              case LowLabelDeclarationPoint => LabelVar(n).setAster(true)
              case _ => TypeVar(n)
            }
          }
          else
            throw ResolverError.typeIsNotDefined(typeAnnotation,n)
      }
    /*case UnionTypeAnnotation(left,right)=>
      UnionLabel(resolveType(typeIdentifierScope,left,labelPosisition = false),
        resolveType(typeIdentifierScope,right,labelPosisition = false))*/
    case _ => throw new NotImplementedError("resolveType not implemented")
  }).setAstNode(typeAnnotation)



  private def resolveMethodDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                       methodDeclaration: MethodDeclarationNode):MethodDeclarationE = {
    val methodLabelScope = new NestedScope(typeIdentifierScope)
    //process each parameter, add to the scope and the process the otherone
    val resolvedLabelVars =  methodDeclaration.mType.typeVars.map(labelVar => {
      if(methodLabelScope.contains(labelVar.name))
        throw ResolverError.variableAlreadyDefined(labelVar,labelVar.name)
      else
        methodLabelScope.add(labelVar.name,if(labelVar.isAster) LowLabelDeclarationPoint else LabelDeclarationPoint)
    })
    MethodDeclarationE(methodDeclaration.name.name,
      MTypeE(
        methodDeclaration.mType.domain.map(st=>resolveAnnotatedFacetedType(methodLabelScope,st)),
        resolveAnnotatedFacetedType(methodLabelScope,methodDeclaration.mType.codomain)
      )).setMethodNameNode(methodDeclaration.name).setAstNode(methodDeclaration)
  }
  private def multiExtend(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                          strings: List[String],
                          declarationPoint: TypeIdentifierDeclarationPoint) : Scope[TypeIdentifierDeclarationPoint] ={
    val newScope = new NestedScope(typeIdentifierScope)
    strings.foreach(x=> newScope.add(x,declarationPoint))
    newScope
  }

  private def resolveAnnotatedFacetedType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                          annotatedFacetedType: AnnotatedFacetedType): STypeE={
    val privateType = resolveType(typeIdentifierScope,annotatedFacetedType.left,labelPosisition = false)
    privateType match {
      case g: TypeE =>
        (annotatedFacetedType.right match{
          case LowLabelNode => STypeE(g,g)
          case HighLabelNode => STypeE(g,ObjectType.top)
          case _ => STypeE(g,
            resolveType(typeIdentifierScope, annotatedFacetedType.right,labelPosisition = true))
        }).setAstNode(annotatedFacetedType)
      case _ => throw ResolverError.invalidTypeForPrivateFacet(annotatedFacetedType)
    }
  }

  private def resolveBuiltinNamedTypes(typeName:String,labelPosition:Boolean): Either[LabelE,String]={
    if(typeName == "Int")
      Left(IntADT)
    else if(typeName=="String")
      Left(StringADT)
    else if(typeName == "Bool")
      Left(BoolADT)
    else if(typeName == "Top")
      Left(ObjectType.top)
    else Right(typeName)
  }
}
object IdentifierResolver{
  def apply(expression: AstNode):EObSecExpr=
    new IdentifierResolver().resolve(expression)
}


sealed trait TypeIdentifierDeclarationPoint
case class TypeDeclarationPoint(var definingType:ObjectType) extends TypeIdentifierDeclarationPoint
case object LabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object LowLabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object AnythingElse extends TypeIdentifierDeclarationPoint


