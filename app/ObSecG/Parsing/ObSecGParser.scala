package ObSecG.Parsing


import ObSec.Parsing.DebugPackratParsers
import ObSecG.Ast
import ObSecG.Ast._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.Position



/**
  * Defining a parser for ObSec language
  */
object ObSecGParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  lexical.reserved += ("if" , "then" , "else" , "true", "false", "let" ,"in", "type", "new", "def","val","deftype", "ot" , "Int" , "String" , "Bool", "StrList" , "L" , "H"  ,"mklist",
    "cons",
    "extends","super","low" )
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

  lazy val symIdentifier:PackratParser[String] =  rep("+"| "-"| "="|"<"|"/") ^^ {case l => l.foldLeft("")((acc,a)=>acc+a)}

  /*lazy val extendedIdentifier : PackratParser[String] =
    (identifier | symIdentifier) ^^ {str => str}*/
  lazy val extendedIdentifier : PackratParser[String] = ident | symIdentifier

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

  def  program: PackratParser[ObSecGAstExprNode] = phrase(expr)//new Wrap("program",phrase(expr))
  lazy val expr : Parser[ObSecGAstExprNode] = {
    valExpr |||  varExpr ||| methodInvExpr | ifThenElse | letStarExpr | mkListExpr | consList
  }

  lazy val consList: Parser[ObSecGAstExprNode] =
    (("cons" ~ LEFTPAREN) ~> ((expr <~ ",") ~ expr)) <~ RIGHTPAREN ^^ {case elem~l => ConsListOperatorNode(elem,l)}

  lazy val mkListExpr : Parser[ObSecGAstExprNode] =
    ((MKLIST ~ LEFTPAREN) ~> repsep(expr,",")) <~ RIGHTPAREN ^^ (l => ListLiteral(l))

  lazy val letStarExpr :PackratParser[ObSecGAstExprNode] =
    ((LET ~ LEFTBRACKET )~> (rep(typeDecl) ~ rep(localDecl))) ~ ((RIGHTBRACKET ~ IN) ~> expr) ^^
      {case  tDecls ~ decls ~ expr => LetStarExpressionNode(tDecls ++ decls,expr)}

  lazy val typeDecl : PackratParser[DeclarationNode] = defTypeDecl | typeAliasDecl
  lazy val typeAliasDecl : PackratParser[TypeAliasDeclarationNode] =
    (("type" ~> ident <~ EQUALSSIGN) ~ objType) ^^ {case id ~ t => TypeAliasDeclarationNode(id,t)}

  lazy val defTypeDecl : PackratParser[DefTypeNode] =
    "deftype" ~> ident ~ (LEFTBRACKET  ~> (methodList <~ RIGHTBRACKET)) ^^ {case tName ~ methodList =>  DefTypeNode(tName,methodList)}


  lazy val localDecl : PackratParser[LocalDeclarationNode] =
    localDecl11 | localDecl12

  lazy val localDecl11 : PackratParser[LocalDeclarationNode] =
    ((identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclarationNode(id,expr)}

  lazy val localDecl12 : PackratParser[LocalDeclarationNode] =
    (("val" ~> identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclarationNode(id,expr)}


  lazy val valExpr :  Parser[ObSecGAstExprNode] = objectExpr | objectExpr2 | primVal

  def varExpr : PackratParser[ObSecGAstExprNode] =  identifier ^^ (vName => VariableNode(vName))


  lazy val methodInvExpr :PackratParser[MethodInvocationNode]= positioned(methodInvExpr2 | methodInvExpr1)

  lazy val methodInvExpr1 :PackratParser[MethodInvocationNode]={
    (positioned(expr) <~ DOT) ~ extendedIdentifier ~ ((LEFTPAREN ~> repsep(positioned(expr), COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ args => MethodInvocationNode(e1,List(),args,id)}
  }
  lazy val methodInvExpr2 :PackratParser[MethodInvocationNode]={
    (expr <~ DOT) ~ extendedIdentifier ~ ((LEFTSBRACKET ~> repsep(labelType,COMMMA)) <~ RIGHTSBRACKET) ~ ((LEFTPAREN ~> repsep(expr, COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ types  ~ args => MethodInvocationNode(e1,types,args,id)}
  }

  lazy val ifThenElse :PackratParser[IfExpressionNode]= {
    (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {case c ~ e1 ~ e2 => IfExpressionNode(c,e1,e2)}
  }

  lazy val  objectExpr: PackratParser[ObSecGAstExprNode] = {
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ positioned(stype) ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET) ^^
      {case self ~ stype ~ methodDefs => ObjectDefinitionNode(self,stype,methodDefs)}
  }
  lazy val  objectExpr2: PackratParser[ObSecGAstExprNode] = {
    "new" ~> (((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET)) ^^
      {case self ~ stype ~ methodDefs => ObjectDefinitionNode(self,stype,methodDefs)}
  }
  lazy val methodDefs : PackratParser[List[MethodDefinitionNode]]={
    rep(methodDef)
  }

  lazy val methodDef : PackratParser[MethodDefinitionNode] = methodDef11 | methodDef12
  lazy val methodDef11 : PackratParser[MethodDefinitionNode] = {
    LEFTBRACKET ~> (extendedIdentifier ~ rep(identifier)) ~ ((EQUALSSIGN ~> expr ) <~ RIGHTBRACKET) ^^
      { case mName ~ args ~ expr => MethodDefinitionNode(mName,args,expr)}
  }
  lazy val methodDef12 : PackratParser[MethodDefinitionNode] = {
    "def" ~> (extendedIdentifier ~ rep(identifier)) ~ ((EQUALSSIGN ~> expr )) ^^
      { case mName ~ args ~ expr => MethodDefinitionNode(mName,args,expr)}
  }
  lazy val stype : PackratParser[AnnotatedFacetedType] = stypeX | publicTypeShortcut

  lazy val stypeX : PackratParser[AnnotatedFacetedType] ={
    ((privateType <~ LESSTHAN) ~ labelType) ^^
      {case t1 ~ t2  => AnnotatedFacetedType(t1,t2)
    }
  }
  lazy val publicTypeShortcut: PackratParser[AnnotatedFacetedType] ={
    privateType ^^ {t1  => AnnotatedFacetedType(t1,t1)}
  }

  lazy val privateType : PackratParser[TypeAnnotation] ={
    positioned(objType  |  primType |  varType)
  }

  lazy val varType : PackratParser[TypeIdentifier] = identifier ^^
    { id => TypeIdentifier(id)}



  lazy val labelType : PackratParser[TypeAnnotation] ={
    positioned(objType | lowLabel | highLabel | primType  | varType)
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



  lazy val lowLabel : PackratParser[TypeAnnotation]= LOW ^^ {_ => TypeIdentifier(LOW)}
  lazy val highLabel : PackratParser[TypeAnnotation]= HIGH ^^ {_ => TypeIdentifier(HIGH)}

  lazy val primVal :  Parser[ObSecGAstExprNode] = stringLiteralExpr | integerExpr | boolExpr

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
    rep(positioned(methodSignature))
  }
  lazy val methodSignature : PackratParser[MethodDeclarationNode]=
    methodSignature1 | methodSignature2

  lazy val methodSignature1 : PackratParser[MethodDeclarationNode]={
    ((LEFTBRACKET ~> extendedIdentifier) ~ ("[" ~> typeVarBounds <~  "]") <~ COLON) ~ (rep(stype) <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName ~ typeParamaters ~ argTypes ~ t2 =>
        MethodDeclarationNode(mName,MethodTypeNode(typeParamaters,argTypes,t2))}
  }

  lazy val methodSignature2 : PackratParser[MethodDeclarationNode]={
    ((LEFTBRACKET ~> extendedIdentifier)  <~ COLON) ~ (rep(stype) <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName  ~ argTypes ~ t2 => MethodDeclarationNode(mName,MethodTypeNode(List(),argTypes,t2))}
  }

  lazy val typeVarBounds : PackratParser[List[LabelVariableDeclarationNode]]={
    repsep(typeVarBound,",")
  }
  lazy val typeVarBound:PackratParser[LabelVariableDeclarationNode]={
    typeVarBoundAster | typeVarBoundNoAster
  }
  lazy val typeVarBoundNoAster:PackratParser[LabelVariableDeclarationNode]={
    upperConstraint | lowerConstraint | boundConstraint
  }
  lazy val typeVarBoundAster:PackratParser[LabelVariableDeclarationNode]={
    ("low" ~> typeVarBound) ^^ (labelVarDefinition => labelVarDefinition.toAster)
  }
  lazy val upperConstraint:PackratParser[SubLabelVariableDeclaration]={
    ((identifier <~ "extends") ~ labelType) ^^
    {case id ~ rawType => SubLabelVariableDeclaration(id,rawType)}
  }

  lazy val lowerConstraint:PackratParser[SuperLabelVariableDeclaration]={
    ((identifier <~ "super") ~ labelType) ^^
      {case id ~ rawType => SuperLabelVariableDeclaration(id,rawType)}
  }
  lazy val boundConstraint:PackratParser[BoundedLabelVariableDeclaration]={
    ((identifier <~ ":") ~ (labelType <~ "..") ~ labelType) ^^
      {case id ~ lower ~ upper => BoundedLabelVariableDeclaration(id,lower,upper)}
  }



  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ObSecParserError, ObSecGAstExprNode] = {
    //parseAll(program,string) match {
    phrase(expr) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseType(string:String):Either[ObSecParserError, TypeAnnotation] = {
    //parseAll(singleType,string) match {
    phrase(privateType) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }
  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseSType(string:String):Either[ObSecParserError, AnnotatedFacetedType] = {
    //parseAll(stype,string) match {
    phrase(stype) (new lexical.Scanner(string)) match {
      case  x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

}

trait ObSecCompilationError
case class ObSecParserError(msg: String,pos:Position,offset:Int) extends ObSecCompilationError


