package ObSecG.Parsing

import Common.{AstNode, NestedScope, Scope, ThrowableAnalysisError}
import ObSecG.Ast._

class ObSecGIdentifierResolver {

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
  def resolve(expression: ObSecGAstExprNode):ObSecGExpr =
    resolve(new Scope,new Scope,expression)

  def resolveType(typeAnnotation: TypeAnnotation):LabelG=
    resolveType(new Scope,typeAnnotation,labelPosisition = true)

  private def resolve(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                      valueIdentifier: Scope[Boolean],
                      expression: ObSecGAstExprNode):ObSecGExpr =
    resolveInternal(typeIdentifierScope,valueIdentifier,expression).setAstNode(expression)



  private def resolveInternal(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                              valueIdentifier: Scope[Boolean],
                              expression: ObSecGAstExprNode):ObSecGExpr =expression match{
    case VariableNode(n) =>
      if(valueIdentifier.contains(n))
        Var(n)
      else
        throw ResolverError.variableNotDefined(expression,n)
    case ObjectDefinitionNode(self,typeAnnotation,methods) =>
      if (methods.map(x => x.name).distinct.lengthCompare(methods.size) == 0) {
        var objectScope = new NestedScope(valueIdentifier)
        objectScope.add(self,true)
        Obj(self,
          resolveAnnotatedFacetedType(typeIdentifierScope, typeAnnotation),
          methods.map(meth => {
            val methodLabelDefinitionScope = new NestedScope(typeIdentifierScope)
            var methodValueVariableScope = new NestedScope(objectScope)

            addMethodLabelVariable(methodLabelDefinitionScope , meth.name, typeAnnotation.left)
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
    case ListLiteral(elems)=>
      ListConstructorExpr(elems.map(e=>resolve(typeIdentifierScope,valueIdentifier,e)))
    case ConsListOperatorNode(e1,e2)=>
      ConsListExpr(resolve(typeIdentifierScope,valueIdentifier,e1),
        resolve(typeIdentifierScope,valueIdentifier,e2))
    case LetStarExpressionNode(declarations,body)=>
      val letTypeScope = new NestedScope(typeIdentifierScope)
      val letValueIdentifierScope = new NestedScope(valueIdentifier)
      LetStarExpr(declarations.map(d => {
        d match {
          case node: LocalDeclarationNode => letValueIdentifierScope.add(node.variable, true)
          case node: TypeAliasDeclarationNode => letTypeScope.add(node.aliasName, AnythingElse)
          case node: DefTypeNode => letTypeScope.add(node.name, AnythingElse)
        }
        resolveDeclaration(letTypeScope,valueIdentifier,d).setAstNode(d)
      }),
        resolve(letTypeScope,letValueIdentifierScope,body))
    case _ => throw new NotImplementedError()
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


  private def addMethodLabelVariable(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                     name: String,
                                     containerType : TypeAnnotation): Unit = containerType match {
    case ObjectTypeNode(self,methods) =>
      if (methods.map(x => x.name).distinct.lengthCompare(methods.size) == 0) {
        val possibleMethods = methods.filter(m => m.name.name == name)
        if (possibleMethods.size == 1) {
          val methodDeclarationG = possibleMethods.head
          methodDeclarationG.mType.typeVars.foreach(labelVar =>
            typeIdentifierScope.add(labelVar.name,if(labelVar.isAster) LowLabelDeclarationPoint else LabelDeclarationPoint)
          )
          Unit
        }
      }
      else
        throw ResolverError.duplicatedMethodInObjectType(methods.reverse.groupBy(identity).collect({case (x,List(_,_,_*)) => x}).head)
    case _ => Unit
  }

  private def resolveMethodDefinition(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                      valueIdentifier: Scope[Boolean],
                                      md: MethodDefinitionNode):MethodDef ={
    MethodDef(md.name,md.args.elems.map(x=>x.name),resolve(typeIdentifierScope,valueIdentifier,md.mBody)).setAstNode(md)
  }

  private def resolveType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                          typeAnnotation: TypeAnnotation,
                          labelPosisition: Boolean):LabelG = (typeAnnotation match{

    case ObjectTypeNode(selfVar,methods) =>
      var objectTypeScope = new NestedScope(typeIdentifierScope)
      objectTypeScope.add(selfVar,SelfDeclarationPoint)
      ObjectType(selfVar,methods.map(m=> resolveMethodDeclaration(typeIdentifierScope,m)))
    case NoRecursiveObjectTypeNode(methods)=> resolveType(typeIdentifierScope,ObjectTypeNode("gen",methods),labelPosisition)
    case TypeIdentifier(n) =>
      val namedType = resolveBuiltinNamedTypes(n,labelPosisition)
      namedType match{
        case Left(t)=>t
        case Right(_) =>
          if(typeIdentifierScope.contains(n)) {
            val definitionPoint = typeIdentifierScope.lookup(n)
            definitionPoint match{
              case SelfDeclarationPoint => TypeVar(n)
              case LabelDeclarationPoint => LabelVar(n)
              case LowLabelDeclarationPoint => LabelVar(n).setAster(true)
              case _ => TypeVar(n)
            }
          }
          else
            throw ResolverError.typeIsNotDefined(typeAnnotation,n)
      }
    case UnionTypeAnnotation(left,right)=>
      UnionLabel(resolveType(typeIdentifierScope,left,labelPosisition = false),
        resolveType(typeIdentifierScope,right,labelPosisition = false))
    case _ => throw new NotImplementedError()
  }).setAstNode(typeAnnotation)



  private def resolveMethodDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                       methodDeclaration: MethodDeclarationNode):MethodDeclarationG = {
    val methodLabelScope = new NestedScope(typeIdentifierScope)
    //process each parameter, add to the scope and the process the otherone
    val resolvedLabelVars =  methodDeclaration.mType.typeVars.map(labelVar => {
      if(methodLabelScope.contains(labelVar.name))
        throw ResolverError.variableAlreadyDefined(labelVar,labelVar.name)
      else
        methodLabelScope.add(labelVar.name,if(labelVar.isAster) LowLabelDeclarationPoint else LabelDeclarationPoint)
        resolveLabelVariableDeclaration(methodLabelScope,labelVar)
    })
    MethodDeclarationG(methodDeclaration.name.name,
      MTypeG(
        resolvedLabelVars,
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
                                          annotatedFacetedType: AnnotatedFacetedType): STypeG={
    val privateType = resolveType(typeIdentifierScope,annotatedFacetedType.left,labelPosisition = false)
    privateType match {
      case g: TypeG =>
        (annotatedFacetedType.right match{
          case LowLabelNode => STypeG(g,g)
          case HighLabelNode => STypeG(g,ObjectType.top)
          case _ => STypeG(g,
            resolveType(typeIdentifierScope, annotatedFacetedType.right,labelPosisition = true))
        }).setAstNode(annotatedFacetedType)
      case _ => throw ResolverError.invalidTypeForPrivateFacet(annotatedFacetedType)
    }
  }
  private def resolveLabelVariableDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                              labelVariableDeclarationNode: LabelVariableDeclarationNode): BoundedLabelVar
  = (labelVariableDeclarationNode match{
    case SimpleLabelVariableDeclarationNode(s)=>
      BoundedLabelVar(s,Bottom,ObjectType.top)
    case BoundedLabelVariableDeclaration(s,lower,upper)=>
      BoundedLabelVar(s,
        resolveType(typeIdentifierScope,lower,labelPosisition = true),
        resolveType(typeIdentifierScope,upper,labelPosisition = true)
      )
    case SubLabelVariableDeclaration(s,upper)=>
      BoundedLabelVar(s,
        Bottom,
        resolveType(typeIdentifierScope,upper,labelPosisition = true)
      )
    case SuperLabelVariableDeclaration(s,lower)=>
      BoundedLabelVar(s,
        resolveType(typeIdentifierScope,lower,labelPosisition = true)
        ,ObjectType.top
      )}).setAstNode(labelVariableDeclarationNode).setAster(labelVariableDeclarationNode.isAster)

  private def resolveBuiltinNamedTypes(typeName:String,labelPosition:Boolean): Either[LabelG,String]={
    if(typeName == "Int")
      Left(IntType)
    else if(typeName=="String")
      Left(StringType)
    else if(typeName == "Bool")
      Left(BooleanType)
    else if(typeName == "StrList")
      Left(StringListType)
    else if(typeName == "Top")
      Left(ObjectType.top)
    else Right(typeName)
  }
}
object ObSecGIdentifierResolver{
  def apply(expression: ObSecGAstExprNode):ObSecGExpr=
    new ObSecGIdentifierResolver().resolve(expression)
}


sealed trait TypeIdentifierDeclarationPoint
case object SelfDeclarationPoint extends TypeIdentifierDeclarationPoint
case object LabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object LowLabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object AnythingElse extends TypeIdentifierDeclarationPoint


