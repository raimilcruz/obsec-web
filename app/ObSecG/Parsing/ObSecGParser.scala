package ObSecG.Parsing


import ObSec.Parsing.DebugPackratParsers
import ObSecG.Ast._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.Position



/**
  * Defining a parser for ObSec language
  */
object ObSecGParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  lexical.reserved += ("if" , "then" , "else" , "true", "false", "let" ,"in", "type", "new", "def","val","deftype", "ot" , "Int" , "String" , "Bool", "StrList" , "L" , "H"  ,"mklist","cons","extends" )
  lexical.delimiters ++= (": . < > -> => + - * / ( ) [ ] { } , = ; <:" split ' ')
/*

  val reserved: PackratParser[String] =
  {"if" | "then" | "else" | "true" | "false" | "Ref" | "Unit" | "ot" | "Int" | "String" | "Boolean" | "L" | "H" | "let" |"in" | "mklist" }
  val reserved2: List[String] =
  List("if" , "then" , "else" , "true" , "false" , "Ref" , "Unit" , "ot" , "Int" , "String" , "Boolean" , "L" , "H" , "let" ,"in" ,"mklist" )
*/

  //lazy val identifier: PackratParser[String] = elem("ident",x=> reserved2.contains(x.toString)) ^^ (x=>x.toString)
  lazy val identifier: PackratParser[String] = ident 

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

  def INT = "Int"
  def STRING = "String"
  def BOOLEAN = "Bool"
  def STRLIST = "StrList"

  def LET ="let"
  def IN ="in"

  def LOW ="L"
  def HIGH ="H"

  def  program: PackratParser[ObSecGExpr] = phrase(expr)//new Wrap("program",phrase(expr))
  lazy val expr : Parser[ObSecGExpr] = {
    valExpr |||  varExpr ||| methodInvExpr | ifThenElse | letStarExpr | mkListExpr | consList
  }

  lazy val consList: Parser[ObSecGExpr] =
    (("cons" ~ LEFTPAREN) ~> ((expr <~ ",") ~ expr)) <~ RIGHTPAREN ^^ {case elem~l => ConsListExpr(elem,l)}

  lazy val mkListExpr : Parser[ObSecGExpr] =
    ((MKLIST ~ LEFTPAREN) ~> repsep(expr,",")) <~ RIGHTPAREN ^^ (l => ListConstructorExpr(l))

  lazy val letStarExpr :PackratParser[ObSecGExpr] =
    ((LET ~ LEFTBRACKET )~> (rep(typeDecl) ~ rep(localDecl))) ~ ((RIGHTBRACKET ~ IN) ~> expr) ^^
      {case  tDecls ~ decls ~ expr => LetStarExpr(tDecls ++ decls,expr)}

  lazy val typeDecl : PackratParser[Declaration] = defTypeDecl | typeAliasDecl
  lazy val typeAliasDecl : PackratParser[TypeAlias] =
    (("type" ~> ident <~ EQUALSSIGN) ~ objType) ^^ {case id ~ t => TypeAlias(id,t)}

  lazy val defTypeDecl : PackratParser[TypeDefinition] =
    "deftype" ~> ident ~ (LEFTBRACKET  ~> (methodList <~ RIGHTBRACKET)) ^^ {case tName ~ methodList =>  TypeDefinition(tName,methodList)}


  lazy val localDecl : PackratParser[LocalDeclaration] =
    localDecl11 | localDecl12

  lazy val localDecl11 : PackratParser[LocalDeclaration] =
    ((identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclaration(id,expr)}

  lazy val localDecl12 : PackratParser[LocalDeclaration] =
    (("val" ~> identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclaration(id,expr)}


  lazy val valExpr :  Parser[ObSecGExpr] = objectExpr | objectExpr2 | primVal

  def varExpr : PackratParser[ObSecGExpr] =  identifier ^^ (vName => Var(vName))


  lazy val methodInvExpr :PackratParser[ObSecGExpr]={
    (expr <~ DOT) ~ (extendedIdentifier) ~ ((LEFTPAREN ~> repsep(expr, COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ args => MethodInv(e1,args,id)}
  }

  lazy val ifThenElse :PackratParser[ObSecGExpr]= {
    (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {case c ~ e1 ~ e2 => IfExpr(c,e1,e2)}
  }

  lazy val  objectExpr: PackratParser[ObSecGExpr] = {
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET) ^^
      {case self ~ stype ~ methodDefs => Obj(self,stype,methodDefs)}
  }
  lazy val  objectExpr2: PackratParser[ObSecGExpr] = {
    "new" ~> (((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET)) ^^
      {case self ~ stype ~ methodDefs => Obj(self,stype,methodDefs)}
  }
  lazy val methodDefs : PackratParser[List[MethodDef]]={
    rep(methodDef)
  }

  lazy val methodDef : PackratParser[MethodDef] = methodDef11 | methodDef12
  lazy val methodDef11 : PackratParser[MethodDef] = {
    LEFTBRACKET ~> (extendedIdentifier ~ rep(identifier)) ~ ((EQUALSSIGN ~> expr ) <~ RIGHTBRACKET) ^^
      { case mName ~ args ~ expr => MethodDef(mName,args,expr)}
  }
  lazy val methodDef12 : PackratParser[MethodDef] = {
    "def" ~> (extendedIdentifier ~ rep(identifier)) ~ ((EQUALSSIGN ~> expr )) ^^
      { case mName ~ args ~ expr => MethodDef(mName,args,expr)}
  }
  lazy val stype : PackratParser[STypeG] = stypeX | publicTypeShortcut
  lazy val stypeX : PackratParser[STypeG] ={
    ((singleType <~ LESSTHAN) ~ labelType) ^^ {case t1 ~ t2  => t2 match {
      case LowLabel => STypeG(t1,t1)
      case HighLabel => STypeG(t1,ObjectType.top)
      case _ => STypeG(t1,t2)
    } }
  }
  lazy val publicTypeShortcut: PackratParser[STypeG] ={
    singleType ^^ {t1  => STypeG(t1,t1)}
  }

  lazy val singleType : PackratParser[TypeG] ={
    objType  |  primType | varType
  }

  lazy val varType : PackratParser[TypeG] = identifier ^^ { id => TypeVar(id)}

  lazy val labelType : PackratParser[TypeG] ={
    objType | lowLabel | highLabel | primType | varType
  }
  lazy val primType : PackratParser[TypeG] = {
    intType | booleanType | stringListType | stringType
  }
  lazy val intType : PackratParser[TypeG] = INT ^^ {_=> IntType}
  lazy val stringType : PackratParser[TypeG] = STRING ^^ {_=> StringType}
  lazy val booleanType : PackratParser[TypeG] = BOOLEAN ^^ {_=> BooleanType}
  lazy val stringListType : PackratParser[TypeG] =  stringListStringType | stringListGType//STRLIST ^^ {_=> StringListType}

  lazy val stringListStringType : PackratParser[TypeG] = STRLIST ^^ {_=> StringListType}
  lazy val stringListGType : PackratParser[TypeG] = (STRLIST~"[") ~> (singleType <~"]") ^^ (t => StringGListType(t))



  lazy val lowLabel : PackratParser[TypeG]= LOW ^^ {_ => LowLabel}
  lazy val highLabel : PackratParser[TypeG]= HIGH ^^ {_ => HighLabel}

  lazy val primVal :  Parser[ObSecGExpr] = stringLiteralExpr | integerExpr | boolExpr

  //lazy val stringLiteralExpr : PackratParser[ObSecGExpr] = stringLit ^^ {s => StringExpr(s.substring(1,s.length-1))}
  lazy val stringLiteralExpr : Parser[ObSecGExpr] = stringLit ^^ {s => StringExpr(s)}
  lazy val integerExpr : Parser[ObSecGExpr] = (numericLit | negativeNumericLiteral) ^^ {s => IntExpr(s.toInt)}
  lazy val negativeNumericLiteral : Parser[String]= "-" ~> numericLit ^^ {s=> "-"+s}
  lazy val boolExpr : PackratParser[ObSecGExpr] = ("true" | "false" ) ^^ {s => BooleanExpr(s == "true")}


  lazy val objType :  PackratParser[ObjectType]= objType11 | objType13 | objType12

  lazy val objType11 :  PackratParser[ObjectType]={
    (LEFTSBRACKET  ~> identifier) ~ (methodList <~ RIGHTSBRACKET) ^^ {case tVar ~ methodList => ObjectType(tVar,methodList)}
  }
  lazy val objType12 :  PackratParser[ObjectType]={
    ((LEFTBRACKET ~ OT) ~> identifier) ~ (methodList <~ RIGHTBRACKET) ^^ {case tVar ~ methodList => ObjectType(tVar,methodList)}
  }
  lazy val objType13 :  PackratParser[ObjectType]={
    LEFTSBRACKET  ~>  (methodList <~ RIGHTSBRACKET) ^^ {case methodList => ObjectType("gen",methodList)}
  }

  lazy val methodList : PackratParser[List[MethodDeclarationG]]={
    rep(methodSignature)
  }
  lazy val methodSignature : PackratParser[MethodDeclarationG]= methodSignature1 | methodSignature2
  lazy val methodSignature1 : PackratParser[MethodDeclarationG]={
    ((LEFTBRACKET ~> extendedIdentifier) ~ ("[" ~> identifier <~ "extends") ~ (singleType <~ "]") <~ COLON) ~ (rep(stype) <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName ~ mTypeVar ~ tVarBound ~ argTypes ~ t2 => MethodDeclarationG(mName,MTypeG(mTypeVar,tVarBound,argTypes,t2))}
  }
  lazy val methodSignature2 : PackratParser[MethodDeclarationG]={
    ((LEFTBRACKET ~> extendedIdentifier)  <~ COLON) ~ (rep(stype) <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName  ~ argTypes ~ t2 => MethodDeclarationG(mName,MTypeG("gen",ObjectType.top,argTypes,t2))}
  }

  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ObSecParserError, ObSecGExpr] = {
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
  def parseType(string:String):Either[ObSecParserError, TypeG] = {
    //parseAll(singleType,string) match {
    phrase(singleType) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }
  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseSType(string:String):Either[ObSecParserError, STypeG] = {
    //parseAll(stype,string) match {
    phrase(stype) (new lexical.Scanner(string)) match {
      case  x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

}

trait ObSecCompilationError
case class ObSecParserError(msg: String,pos:Position,offset:Int) extends ObSecCompilationError


