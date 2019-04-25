package ObSecE.Parsing

import Common._
import ObSecE.Ast._

class EOBSecIdentifierResolver {

  /**
    * This method performs the following tasks:
    * 1. Resolves all TypeIdentifier node to:
    *  a)LabelVar is the type variable references a label variable definition
    *  b)TypeVar is the type variable references a self variable or a type alias
    *
    * 2. Returns the ast model, compacting several surface expressions
    * 3. Checks trivial conditions:
    *   a)Repeated methods
    *   b)Repeated type variables
    * @param expression The ast model
    * @return
    */
  def resolve(expression: AstNode):EObSecExpr =
    resolve(new Scope,new Scope,expression)

  def resolveType(typeAnnotation: TypeAnnotation):LabelE=
    resolveType(new Scope,typeAnnotation,labelPosisition = true)

  def resolveAnnotatedFacetedType(annotatedFacetedType: AnnotatedFacetedType): STypeE=
    resolveAnnotatedFacetedType(new Scope,annotatedFacetedType)

  //exposed for unit-testing purposes
  def resolve(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
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
            val methodExistentialVariablesScope = new NestedScope(typeIdentifierScope)
            var methodValueVariableScope = new NestedScope(objectScope)

            //add existential variables of formal parameters to scope.
            addExistentialVariablesOfMethodArguments(
              methodExistentialVariablesScope,
              meth.name,resolvedSelfType.privateType)
            //add method to scope
            meth.args.elems.foreach(x=> liftError(methodValueVariableScope.add(x.name,true),x))

            resolveMethodDefinition(methodExistentialVariablesScope ,methodValueVariableScope, meth)
          })
        )
      }
      else{
        print(methods)
        throw ResolverError.duplicatedMethodInObject(methods.reverse.groupBy(identity).collect({case (x,List(_,_,_*)) => x}).head)
      }
    case MethodInvocationNode(e1,actualArguments,name)=>
      MethodInv(
        resolve(typeIdentifierScope,valueIdentifier,e1),
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
            val resolvedType = resolveType(letTypeScope,node.objType,labelPosisition = true)
            letTypeScope.add(node.aliasName, TypeAliasDeclarationPoint(resolvedType))
          case node: DefTypeNode =>
            val defTypeDeclaration = TypeDefDeclarationPoint(null)
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



  private def addExistentialVariablesOfMethodArguments(typeIdentifierScope: NestedScope[TypeIdentifierDeclarationPoint],
                                                       name: String, theType: LabelE) =
    typeFromScope(typeIdentifierScope,theType) match{
      case ObjectType(self,methods)=>
        methods.find(m => m.name == name) match {
          case Some(methodDeclarationG)=>
            methodDeclarationG.mType.domain
              .filter(x=> x.isInstanceOf[ESTypeE]).map(x=> x.asInstanceOf[ESTypeE])
              .foreach(existentialST =>
                  addExistentialVariablesOfExistentialST(typeIdentifierScope,existentialST)
            )
          case None => Unit
       }
      case _ => Unit
  }

  private def typeFromScope(typeIdentifierScope: NestedScope[TypeIdentifierDeclarationPoint],
                            theType: LabelE) = theType match{
    case tI@TypeId(name)=>
      if(!typeIdentifierScope.contains(name))
        throw ResolverError.typeIsNotDefined(tI.astNode,name)
      typeIdentifierScope.lookup(name) match {
        case TypeAliasDeclarationPoint(definingType)=> definingType
        case TypeDefDeclarationPoint(definingType)=> definingType
        case _ => throw ResolverError.generalError(tI.astNode,"Internal error: a type identifier" +
          "should point to a type alias or type definition")
      }
    case t => t
  }
  private def addExistentialVariablesOfExistentialST(typeIdentifierScope: NestedScope[TypeIdentifierDeclarationPoint],
                                                     existentialST: ESTypeE): Unit = existentialST match{
    case ESTypeE(priv,impl,existentialFacet) =>
      typeFromScope(typeIdentifierScope,existentialFacet) match{
        case ExistentialType(typeVars,methods)=>
          typeVars.foreach(existentialVar =>
            typeIdentifierScope.add(existentialVar.name,
              if(existentialVar.isAster)
                LowLabelDeclarationPoint
              else LabelDeclarationPoint)
          )
        case _ => Unit
      }
  }

  private def resolveDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                         valueIdentifier: Scope[Boolean],
                         declaration: DeclarationNode): Declaration= declaration match{
    case DefTypeNode(typeName,methods)=>
      TypeDefinition(typeName,methods.map(m=> resolveMethodDeclaration(typeIdentifierScope,m)))
    case TypeAliasDeclarationNode(typeAlias,aliasType)=>
      TypeAlias(typeAlias,resolveType(typeIdentifierScope,aliasType,labelPosisition = false))
    case LocalDeclarationNode(name,expr)=>
      LocalDeclaration(name,resolve(typeIdentifierScope,valueIdentifier,expr))
  }



  private def resolveMethodDefinition(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                      valueIdentifier: Scope[Boolean],
                                      md: MethodDefinitionNode):MethodDef ={
    MethodDef(md.name,md.args.elems.map(x=>x.name),resolve(typeIdentifierScope,valueIdentifier,md.mBody)).setAstNode(md)
  }

  def checkDuplicatedMethodDeclarationName(methods: List[MethodDeclarationNode]):Unit = {
    if (methods.map(x => x.name).distinct.lengthCompare(methods.size) != 0)
      throw ResolverError.duplicatedMethodInObjectType(methods.reverse.groupBy(identity).collect({case (x,List(_,_,_*)) => x}).head)
  }

  def resolveType(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                          typeAnnotation: TypeAnnotation,
                          labelPosisition: Boolean):LabelE = (typeAnnotation match{

    case ObjectTypeNode(selfVar,methods) =>
      checkDuplicatedMethodDeclarationName(methods)
      val objectTypeScope = new NestedScope(typeIdentifierScope)
      val selfDeclarationPoint  = ObjectDeclarationPoint(null)
      objectTypeScope.add(selfVar,selfDeclarationPoint)
      val objectType = ObjectType(selfVar,methods.map(m=> resolveMethodDeclaration(objectTypeScope,m)))
      selfDeclarationPoint.definingOhjectType = objectType
      objectType
    case NoRecursiveObjectTypeNode(methods)=> resolveType(typeIdentifierScope,ObjectTypeNode("gen",methods),labelPosisition)
    case TypeIdentifier(n) =>

      def typeFromDefinitionPoint(definitionPoint: TypeIdentifierDeclarationPoint) = {
        definitionPoint match {
          case TypeAliasDeclarationPoint(_) => TypeId(n)
          case TypeDefDeclarationPoint(_) => TypeId(n)
          case LabelDeclarationPoint => LabelVar(n)
          case LowLabelDeclarationPoint => LabelVar(n).setAster(true)
          case ObjectDeclarationPoint(_) => TypeVar(n)
        }
      }
      val namedType = resolveBuiltinNamedTypes(n,labelPosisition)
      namedType match{
        case Left(t)=>t
        case Right(_) =>
          if(typeIdentifierScope.contains(n)) {
            typeFromDefinitionPoint(typeIdentifierScope.lookup(n))
          }
          else
            throw ResolverError.typeIsNotDefined(typeAnnotation,n)
      }
    case ExistentialTypeNode(typeVars,methods)=>
      checkDuplicatedMethodDeclarationName(methods)
      val existentialTypeScope = new NestedScope(typeIdentifierScope)
      //add variables to scope
      typeVars.elems.foreach(tv => {
        if(existentialTypeScope.contains(tv.name))
          throw ResolverError.variableAlreadyDefined(tv,tv.name)
        else
            existentialTypeScope.add(tv.name,
              if(tv.isAster)
                LowLabelDeclarationPoint
              else LabelDeclarationPoint)
        }
      )
      val existentialType =
        ExistentialType(
          typeVars.elems.map(x=>EVarDecl(x.name,
            resolveType(typeIdentifierScope,x.lowerBound,labelPosisition=true).asInstanceOf[TypeE]).setAster(x.isAster)),
          methods.map(m=> resolveMethodDeclaration(existentialTypeScope,m)))
      existentialType
    case _ => throw new NotImplementedError("resolveType not implemented")
  }).setAstNode(typeAnnotation)



  private def resolveMethodDeclaration(typeIdentifierScope: Scope[TypeIdentifierDeclarationPoint],
                                       methodDeclaration: MethodDeclarationNode):MethodDeclarationE = {
    val methodLabelScope = new NestedScope(typeIdentifierScope)
    //process each parameter, add to the scope and the process the otherone
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
                                          annotatedFacetedType: AnnotatedFacetedType): STypeE= annotatedFacetedType match{
    case AnnotatedSubtypingFacetedType(left,right)=>
      val privateType = resolveType(typeIdentifierScope,left,labelPosisition = false)
      privateType match {
        case g: TypeE =>
          (right match{
            case LowLabelNode => FTypeE(g,g)
            case HighLabelNode => FTypeE(g,ObjectType.top)
            case _ => FTypeE(g,
              resolveType(typeIdentifierScope, right,labelPosisition = true))
          }).setAstNode(annotatedFacetedType)
        case _ => throw ResolverError.invalidTypeForPrivateFacet(annotatedFacetedType)
      }
    case AnnotatedExistentialFacetedType(concrete,impl,existential) =>
      val privateType = resolveType(typeIdentifierScope,concrete,labelPosisition = false)
      val implTypes = impl.map(x => resolveType(typeIdentifierScope, x,labelPosisition = false))
      var existentialType = resolveType(typeIdentifierScope, existential,labelPosisition = true)
      (privateType,implTypes,existentialType) match {
        case (g: TypeE,i:List[TypeE],e:ExistentialFacet) =>
          ESTypeE(g,i,e).setAstNode(annotatedFacetedType)
        case (_,i:TypeE,e:ExistentialFacet) => throw ResolverError.invalidTypeForPrivateFacet(annotatedFacetedType)
        case (g: TypeE,_,e:ExistentialFacet) => throw EObSecResolverError.invalidTypeForImplementationType(annotatedFacetedType)
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
object EOBSecIdentifierResolver{
  def apply(expression: AstNode):EObSecExpr=
    new EOBSecIdentifierResolver().resolve(expression)
  def apply(typeNode: TypeAnnotation): LabelE =
    new EOBSecIdentifierResolver().resolveType(typeNode)
  def apply(typeNode: AnnotatedFacetedType): STypeE =
    new EOBSecIdentifierResolver().resolveAnnotatedFacetedType(typeNode)
}


sealed trait TypeIdentifierDeclarationPoint
case class TypeAliasDeclarationPoint(var definingType:LabelE) extends TypeIdentifierDeclarationPoint
case class TypeDefDeclarationPoint(var definingType:ObjectType) extends TypeIdentifierDeclarationPoint
case class ObjectDeclarationPoint(var definingOhjectType: ObjectType) extends TypeIdentifierDeclarationPoint
case object LabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object LowLabelDeclarationPoint extends TypeIdentifierDeclarationPoint
case object AnythingElse extends TypeIdentifierDeclarationPoint


