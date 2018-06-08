package ObSecG.Parsing

import Common.{NestedScope, Scope}
import ObSecG.Ast._

class ObSecGTypeIdentifierResolver {
  def resolve(expression: ObSecGAstExprNode):ObSecGExpr =
    resolve(new Scope,expression)

  private def resolve(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                      expression: ObSecGAstExprNode):ObSecGExpr =expression match{
    case VariableNode(n) => Var(n)
    case ObjectDefinitionNode(self,typeAnnotation,methods) =>
      Obj(self,
        resolveAnnotatedFacetedType(typeIdentifierScope,typeAnnotation),
        methods.map(meth => {
          val methodDefinitionScope = new NestedScope(typeIdentifierScope)
          addMethodLabelVariable(typeIdentifierScope,meth.name,typeAnnotation.left)
          resolveMethodDefinition(methodDefinitionScope,meth)})
      )
    case MethodInvocationNode(e1,actualTypes,actualArguments,name)=>
      MethodInv(
        resolve(typeIdentifierScope,e1),
        actualTypes.map(at=> resolveType(typeIdentifierScope,at,labelPosisition = true)),
        actualArguments.map(aa => resolve(typeIdentifierScope,aa)),
        name
      )
    case BooleanLiteral(b) => BooleanExpr(b)
    case IntLiteral(n) => IntExpr(n)
    case StringLiteral(s) => StringExpr(s)
    case IfExpressionNode(c,e1,e2)=>
      IfExpr(resolve(typeIdentifierScope,c),
        resolve(typeIdentifierScope,e1),
        resolve(typeIdentifierScope,e2))
    case ListLiteral(elems)=>
      ListConstructorExpr(elems.map(e=>resolve(typeIdentifierScope,e)))
    case ConsListOperatorNode(e1,e2)=>
      ConsListExpr(resolve(typeIdentifierScope,e1),
        resolve(typeIdentifierScope,e2))
    case LetStarExpressionNode(declarations,body)=>
        LetStarExpr(declarations.map(d => resolveDeclaration(typeIdentifierScope,d)),
          resolve(typeIdentifierScope,body))

    case _ => throw new NotImplementedError()
  }

  def resolveDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                         declaration: DeclarationNode): Declaration= declaration match{
    case DefTypeNode(typeName,methods)=>
      TypeDefinition(typeName,methods.map(m=> resolveMethodDeclaration(typeIdentifierScope,m)))
    case TypeAliasDeclarationNode(typeAlias,objType)=>
      TypeAlias(typeAlias,resolveType(typeIdentifierScope,objType,labelPosisition = false).asInstanceOf[ObjectType])
    case LocalDeclarationNode(name,expr)=>
      LocalDeclaration(name,resolve(typeIdentifierScope,expr))
  }

  private def addMethodLabelVariable(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                     name: String,
                                     containerType : TypeAnnotation): Unit = containerType match {
    case ObjectTypeNode(self,methods) =>
      val possibleMethods = methods.filter(m => m.name == name)
      if(possibleMethods.size ==1){
        val methodDeclarationG =  possibleMethods.head
        methodDeclarationG.mType.typeVars.foreach(labelVar =>
          typeIdentifierScope.add(labelVar.name,LabelDeclarationPoint)
        )
        Unit
      }
    case _ => Unit
  }

  private def resolveMethodDefinition(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                      md: MethodDefinitionNode):MethodDef ={

    MethodDef(md.name,md.args,resolve(typeIdentifierScope,md.mBody))
  }

  private def resolveType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                            typeAnnotation: TypeAnnotation, labelPosisition: Boolean):LabelG = typeAnnotation match{

    case ObjectTypeNode(selfVar,methods) =>
      var objectTypeScope = new NestedScope(typeIdentifierScope)
      objectTypeScope.add(selfVar,SelfDeclarationPoint)
      ObjectType(selfVar,methods.map(m=> resolveMethodDeclaration(typeIdentifierScope,m)))
    case NoRecursiveObjectTypeNode(methods)=> resolveType(typeIdentifierScope,ObjectTypeNode("gen",methods),labelPosisition)
    case TypeIdentifier(n) =>
      val namedType = resolveBuiltinNamedTypes(n)
      namedType match{
        case Left(t)=>t
        case Right(_) =>
          if(typeIdentifierScope.contains(n)) {
            val definitionPoint = typeIdentifierScope.lookup(n)
            definitionPoint match{
              case SelfDeclarationPoint => TypeVar(n)
              case LabelDeclarationPoint => LabelVarImpl(n)
              case _ => TypeVar(n)
            }
          }
          else
            throw new Error(s"Type $n is not defined")
      }
    case _ => throw new NotImplementedError()
  }



  private def resolveMethodDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                       methodDeclaration: MethodDeclarationNode):MethodDeclarationG = {
    //TODO: Add new defined labels to scope
    MethodDeclarationG(methodDeclaration.name,
      MTypeG(
        methodDeclaration.mType.typeVars.map(v=>resolveLabelVariableDeclaration(typeIdentifierScope,v)),
        methodDeclaration.mType.domain.map(st=>resolveAnnotatedFacetedType(typeIdentifierScope,st)),
        resolveAnnotatedFacetedType(typeIdentifierScope,methodDeclaration.mType.codomain)
      ))
  }
  private def multiExtend(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                          strings: List[String],
                          declarationPoint: TypeIdentifierDeclarationPoint) : Scope[TypeIdentifierDeclarationPoint ={
    val newScope = new NestedScope(typeIdentifierScope)
    strings.foreach(x=> newScope.add(x,declarationPoint))
    newScope
  }

  private def resolveAnnotatedFacetedType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                          annotatedFacetedType: AnnotatedFacetedType): STypeG={
    val privateType = resolveType(typeIdentifierScope,annotatedFacetedType.left,labelPosisition = false)
    privateType match {
      case g: TypeG =>
        STypeG(g,
          resolveType(typeIdentifierScope, annotatedFacetedType.right,labelPosisition = true))
      case _ => throw new Error("Invalid type in private facet. Private facet type just support: object types, self type variables and builtin types")
    }
  }
  private def resolveLabelVariableDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                              labelVariableDeclarationNode: LabelVariableDeclarationNode): BoundedTypeVar
  = labelVariableDeclarationNode match{
    case SimpleLabelVariableDeclarationNode(s)=>
      BoundedTypeVarImpl(s,Bottom,ObjectType.top)
    case BoundedLabelVariableDeclaration(s,lower,upper)=>
      BoundedTypeVarImpl(s,
        resolveType(typeIdentifierScope,lower,labelPosisition = true),
        resolveType(typeIdentifierScope,upper,labelPosisition = true)
      )
    case SubLabelVariableDeclaration(s,upper)=>
      BoundedTypeVarImpl(s,
        Bottom,
        resolveType(typeIdentifierScope,upper,labelPosisition = true)
      )
    case SuperLabelVariableDeclaration(s,lower)=>
      BoundedTypeVarImpl(s,
        resolveType(typeIdentifierScope,lower,labelPosisition = true)
        ,ObjectType.top
      )
  }
  private def resolveBuiltinNamedTypes(typeName:String): Either[LabelG,String]={
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
    else if(typeName == "L")
      Left(LowLabel)
    else if(typeName == "H")
      Left(HighLabel)
    else Right(typeName)
  }
}
object ObSecGTypeIdentifierResolver{
  def apply(expression: ObSecGAstExprNode):ObSecGExpr=
    new ObSecGTypeIdentifierResolver().resolve(expression)
}


sealed trait TypeIdentifierDeclarationPoint
case object SelfDeclarationPoint extends TypeIdentifierDeclarationPoint
case object LabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object AnythingElse extends TypeIdentifierDeclarationPoint