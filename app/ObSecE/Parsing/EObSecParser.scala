package ObSecE.Parsing

import Common.{DebugPackratParsers, OffsetPositional, ParserError}
import ObSecE.Ast._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.input.NoPosition


/**
  * Defining a parser for ObSec language
  */
object EObSecParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  lexical.reserved += ("if" , "then" , "else" , "true", "false", //booleans
    "let" ,"in", "type","val","deftype", //syntactic sugar to easy type and value declarations
    "ot" ,"new", "def", //object instances
    "exists","with","as" ,"glob", "super" ,//existential faceted types
    "Int" , "String" , "Bool", "StrList" , //primitive types
    "L" , "H"  , //label shortcuts
    "mklist","cons",  //builtin lists
    "I", "extends","super","low" // generic labels
  )
  lexical.delimiters ++= (": . < > -> => + - * / ( ) [ ] { } , = ; <: .." split ' ')
/*

  val reserved: PackratParser[String] =
  {"if" | "then" | "else" | "true" | "false" | "Ref" | "Unit" | "ot" | "Int" | "String" | "Boolean" | "L" | "H" | "let" |"in" | "mklist" }
  val reserved2: List[String] =
  List("if" , "then" , "else" , "true" , "false" , "Ref" , "Unit" , "ot" , "Int" , "String" , "Boolean" , "L" , "H" , "let" ,"in" ,"mklist" )
*/

  //lazy val identifier: PackratParser[String] = elem("ident",x=> reserved2.contains(x.toString)) ^^ (x=>x.toString)
  lazy val identifier: PackratParser[String] = ident
  lazy val gidentifier: PackratParser[String] = "T" ~> ident ^^ {id => "T"+ id}

  lazy val symIdentifier:PackratParser[SimpleIdentifier] =  rep("+"| "-"| "="|"<"|"/") ^^ (l => SimpleIdentifier(l.foldLeft("")((acc, a) => acc + a)))

  /*lazy val extendedIdentifier : PackratParser[String] =
    (identifier | symIdentifier) ^^ {str => str}*/
  lazy val simpleIdentifier : PackratParser[SimpleIdentifier] = ident ^^ (id => SimpleIdentifier(id))
  lazy val extendedIdentifier : PackratParser[SimpleIdentifier] =  myPositioned(simpleIdentifier) | myPositioned(symIdentifier)

  //PARSERS FOR tokens
  def OT = "ot"
  def ARROW = "->"
  def RARROW = "=>"
  def COLON = ":"
  def LEFTBRACKET = "{"
  def RIGHTBRACKET = "}"
  def LEFTSBRACKET = "["
  def RIGHTSBRACKET = "]"
  def LEFTPAREN = "("
  def RIGHTPAREN = ")"
  def LESSTHAN = "<"
  def LANGLE = "<"
  def RANGLE = "<"
  def DOT = "."
  def COMMMA =","
  def EQUALSSIGN = "="
  def IF = "if"
  def THEN = "then"
  def ELSE = "else"
  def TYPEKEYWORD = "type"
  def MKLIST = "mklist"

  def TOP = "Top"
  def INT = "Int"
  def STRING = "String"
  def BOOLEAN = "Bool"
  def STRLIST = "StrList"

  def LET ="let"
  def IN ="in"

  def LOW ="L"
  def HIGH ="H"

  def IMPLICIT_LABEL ="I"


  implicit def myPositioned[T <: OffsetPositional](p: => Parser[T]): Parser[T] = Parser(in => p(in) match {
    case Success(t, in1) =>
      Success(
        if (t.pos == NoPosition)
          (((t setPos in.pos) setOffSet in.offset)
            setEndPos in1.pos)
            setSource in.source.subSequence(in.offset,in1.offset) else t, in1)
    case ns: NoSuccess => ns
  })

  implicit def listToNodeList[T <: EObSecNode](list:List[T]) = AstNodeList(list)
  //implicit def listToNodeList2[T <: AstNode](list:List[T]):AstNodeList[T] = AstNodeList(list)

  def  program: PackratParser[EObSecNode] = phrase(expr)//new Wrap("program",phrase(expr))
  lazy val expr : Parser[EObSecNode] = {
    myPositioned(valExpr) |||  varExpr ||| methodInvExpr | ifThenElse | letStarExpr | mkListExpr | consList
  }

  lazy val consList: Parser[EObSecNode] =
    (("cons" ~ LEFTPAREN) ~> ((expr <~ ",") ~ expr)) <~ RIGHTPAREN ^^ {case elem~l => ConsListOperatorNode(elem,l)}

  lazy val mkListExpr : Parser[EObSecNode] =
     MKLIST ~> ((LEFTSBRACKET ~> myPositioned(labelType)) <~ RIGHTSBRACKET) ~ ((LEFTPAREN ~> repsep(myPositioned(expr), ",")) <~ RIGHTPAREN)^^
      {case label~ l => ListLiteral(label,l)}

  lazy val letStarExpr :PackratParser[EObSecNode] =
    ((LET ~ LEFTBRACKET )~> (rep(myPositioned(typeDecl)) ~ rep(myPositioned(localDecl)))) ~ ((RIGHTBRACKET ~ IN) ~> myPositioned(expr)) ^^
      {case  tDecls ~ decls ~ expr => LetStarExpressionNode(tDecls ++ decls,expr)}

  lazy val typeDecl : PackratParser[DeclarationNode] = myPositioned(defTypeDecl) | myPositioned(typeAliasDecl)
  lazy val typeAliasDecl : PackratParser[TypeAliasDeclarationNode] =
    (("type" ~> ident <~ EQUALSSIGN) ~ ( objType | existentialType )) ^^ {case id ~ t => TypeAliasDeclarationNode(id,t)}

  lazy val defTypeDecl : PackratParser[DefTypeNode] =
    "deftype" ~> ident ~ (LEFTBRACKET  ~> (methodList <~ RIGHTBRACKET)) ^^ {case tName ~ methodList =>  DefTypeNode(tName,methodList)}


  lazy val localDecl : PackratParser[LocalDeclarationNode] =
    localDecl11 | localDecl12

  lazy val localDecl11 : PackratParser[LocalDeclarationNode] =
    ((identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclarationNode(id,expr)}

  lazy val localDecl12 : PackratParser[LocalDeclarationNode] =
    (("val" ~> identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclarationNode(id,expr)}


  lazy val valExpr :  Parser[EObSecNode] = myPositioned(objectExpr) | myPositioned(objectExpr2) | myPositioned(primVal)

  def varExpr : PackratParser[EObSecNode] =  identifier ^^ (vName => VariableNode(vName))


  lazy val methodInvExpr :PackratParser[MethodInvocationNode]= myPositioned(methodInvExpr1)

  lazy val methodInvExpr1 :PackratParser[MethodInvocationNode]={
    (myPositioned(expr) <~ DOT) ~ extendedIdentifier ~ ((LEFTPAREN ~> repsep(myPositioned(expr), COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ args => MethodInvocationNode(e1,args,id)}
  }


  lazy val ifThenElse :PackratParser[IfExpressionNode]= {
    (IF ~> myPositioned(expr)) ~ (THEN ~> myPositioned(expr)) ~ (ELSE ~> myPositioned(expr)) ^^ {case c ~ e1 ~ e2 => IfExpressionNode(c,e1,e2)}
  }

  lazy val  objectExpr: PackratParser[EObSecNode] = {
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ myPositioned(stype) ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET) ^^
      {case self ~ sType ~ methodDefs => ObjectDefinitionNode(self,sType,methodDefs)}
  }
  lazy val  objectExpr2: PackratParser[EObSecNode] = {
    "new" ~> (((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET)) ^^
      {case self ~ stype ~ methodDefs => ObjectDefinitionNode(self,stype,methodDefs)}
  }
  lazy val methodDefs : PackratParser[List[MethodDefinitionNode]]={
    rep(myPositioned(methodDef))
  }

  lazy val methodDef : PackratParser[MethodDefinitionNode] = myPositioned(methodDef11) | myPositioned(methodDef12)
  lazy val methodDef11 : PackratParser[MethodDefinitionNode] = {
    LEFTBRACKET ~> (extendedIdentifier ~ rep(myPositioned(simpleIdentifier))) ~ ((EQUALSSIGN ~> myPositioned(expr) ) <~ RIGHTBRACKET) ^^
      { case mName ~ args ~ expr => MethodDefinitionNode(mName,args,expr)}
  }
  lazy val methodDef12 : PackratParser[MethodDefinitionNode] = {
    "def" ~> (extendedIdentifier ~ rep(myPositioned(simpleIdentifier))) ~ ((EQUALSSIGN ~> myPositioned(expr) )) ^^
      { case mName ~ args ~ expr => MethodDefinitionNode(mName,args,expr)}
  }
  lazy val stype : PackratParser[AnnotatedFacetedType] = myPositioned(stypeSubtyping | stypeExistential | publicTypeShortcut )

  lazy val stypeSubtyping : PackratParser[AnnotatedFacetedType] ={
    ((myPositioned(privateType) <~ LESSTHAN) ~ myPositioned(labelType)) ^^
      {case t1 ~ t2  => AnnotatedSubtypingFacetedType(t1,t2)
    }
  }

  lazy val stypeExistential : PackratParser[AnnotatedExistentialFacetedType] ={
    (((myPositioned(privateType) <~ "with") ~ repsep(myPositioned(privateType),",") <~ "as") ~ myPositioned(existentialTypeFacet)) ^^
      {case t1 ~ implTypes ~ t2  => AnnotatedExistentialFacetedType(t1,implTypes,t2)
      }
  }
  lazy val existentialTypeFacet : PackratParser[TypeAnnotation] =
    myPositioned(varType | existentialType)

  lazy val publicTypeShortcut: PackratParser[AnnotatedSubtypingFacetedType] ={
    myPositioned(privateType) ^^ {t1  => AnnotatedSubtypingFacetedType(t1,t1)}
  }

  lazy val privateType : PackratParser[TypeAnnotation] ={
    myPositioned(objType  |  primType |  varType)
  }

  lazy val varType : PackratParser[TypeIdentifier] = identifier ^^
    { id => TypeIdentifier(id)}

  lazy val existentialType : PackratParser[ExistentialTypeNode] =
    ("exists" ~> repsep(myPositioned(existentialVarDeclaration),",") <~ ".") ~ (LEFTSBRACKET ~> methodList <~ RIGHTSBRACKET) ^^
      {case tVarList ~ record => ExistentialTypeNode(tVarList,record)}

  lazy val existentialVarDeclaration:PackratParser[ExistentialVariableDeclarationNode]=
    myPositioned(existentialVarGlobal | existentialVarNoGlobal)

  lazy val existentialVarNoGlobal:PackratParser[ExistentialVariableDeclarationNode]={
    (identifier <~"super")~ myPositioned(labelType)  ^^
      {case typeVarName ~ lowerBound => ExistentialVariableDeclarationNode(typeVarName,lowerBound)}
  }
  lazy val existentialVarGlobal:PackratParser[ExistentialVariableDeclarationNode]={
   ("glob" ~> myPositioned(existentialVarNoGlobal)) ^^ (labelVarDefinition => labelVarDefinition.toAster)
  }




  lazy val labelType : PackratParser[TypeAnnotation] ={
    myPositioned(objType) |  myPositioned(lowLabel) | myPositioned(highLabel) |
      myPositioned(primType)  | myPositioned(varType) | myPositioned(existentialType)
  }

  lazy val primType : PackratParser[TypeAnnotation] = {
    intType | booleanType | stringListType | stringType
  }
  lazy val intType : PackratParser[TypeAnnotation] = INT ^^ {_=> TypeIdentifier(INT)}
  lazy val stringType : PackratParser[TypeAnnotation] = STRING ^^ {_=> TypeIdentifier(STRING)}
  lazy val booleanType : PackratParser[TypeAnnotation] = BOOLEAN ^^ {_=> TypeIdentifier(BOOLEAN)}
  lazy val stringListType : PackratParser[TypeAnnotation] =  stringListStringType //| stringListGType//STRLIST ^^ {_=> StringListType}

  lazy val stringListStringType : PackratParser[TypeAnnotation] = STRLIST ^^ {_=>  TypeIdentifier(STRLIST)}
  //lazy val stringListGType : PackratParser[TypeAnnotation] = (STRLIST~"[") ~> (privateType <~"]") ^^ (t => StringGListType(t))



  lazy val lowLabel : PackratParser[TypeAnnotation]= LOW ^^ {_ => LowLabelNode}
  lazy val highLabel : PackratParser[TypeAnnotation]= HIGH ^^ {_ => HighLabelNode}

  lazy val primVal :  Parser[EObSecNode] =
    myPositioned(stringLiteralExpr) | myPositioned(integerExpr) | myPositioned(boolExpr)

  //lazy val stringLiteralExpr : PackratParser[ObSecGExpr] = stringLit ^^ {s => StringExpr(s.substring(1,s.length-1))}
  lazy val stringLiteralExpr : Parser[StringLiteral] = stringLit ^^ {s => StringLiteral(s)}
  lazy val integerExpr : Parser[IntLiteral] = (numericLit | negativeNumericLiteral) ^^ {s => IntLiteral(s.toInt)}
  lazy val negativeNumericLiteral : Parser[String]= "-" ~> numericLit ^^ {s=> "-"+s}
  lazy val boolExpr : PackratParser[BooleanLiteral] = ("true" | "false" ) ^^ {s => BooleanLiteral(s == "true")}


  lazy val objType :  PackratParser[ObjectTypeAnnotation]= objType11 | objType13 | objType12

  lazy val objType11 :  PackratParser[ObjectTypeAnnotation]={
    (LEFTSBRACKET  ~> identifier) ~ (methodList <~ RIGHTSBRACKET) ^^ {case tVar ~ mList=> ObjectTypeNode(tVar,mList)}
  }
  lazy val objType12 :  PackratParser[ObjectTypeAnnotation]={
    ((LEFTBRACKET ~ OT) ~> identifier) ~ (methodList <~ RIGHTBRACKET) ^^ {case tVar ~ mList => ObjectTypeNode(tVar,mList)}
  }
  lazy val objType13 :  PackratParser[ObjectTypeAnnotation]={
    LEFTSBRACKET  ~>  (methodList <~ RIGHTSBRACKET) ^^ (mList => NoRecursiveObjectTypeNode(mList))
  }

  lazy val methodList : PackratParser[List[MethodDeclarationNode]]={
    rep(myPositioned(methodSignature))
  }
  lazy val methodSignature : PackratParser[MethodDeclarationNode]=
    myPositioned(methodSignature2)

  lazy val methodSignature2 : PackratParser[MethodDeclarationNode]={
    ((LEFTBRACKET ~> extendedIdentifier)  <~ COLON) ~ (rep(myPositioned(stype)) <~ ARROW) ~ (myPositioned(stype) <~ RIGHTBRACKET) ^^
      {case  mName  ~ argTypes ~ t2 => MethodDeclarationNode(mName,MethodTypeNode(argTypes,t2))}
  }



  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ParserError, EObSecNode] = {
    //parseAll(program,string) match {
    phrase(expr) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseType(string:String):Either[ParserError, TypeAnnotation] = {
    //parseAll(singleType,string) match {
    phrase(privateType) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }
  def parseLabelType(string:String):Either[ParserError, TypeAnnotation] = {
    //parseAll(singleType,string) match {
    phrase(labelType) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }
  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseSType(string:String):Either[ParserError, AnnotatedFacetedType] = {
    //parseAll(stype,string) match {
    phrase(stype) (new lexical.Scanner(string)) match {
      case  x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

}




