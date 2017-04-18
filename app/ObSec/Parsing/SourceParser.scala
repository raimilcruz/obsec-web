package ObSec.Parsing

import ObSec.Ast._
import ObSec.Parsing.ObSecParser.rep1

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.Position



/**
  * Defining a parser for ObSec language
  */
object ObSecParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  lexical.reserved += ("if" , "then" , "else" , "true", "false", "let" ,"in", "type", "new", "def","val","deftype", "ot" , "Int" , "String" , "Bool", "StrList" , "L" , "H"  ,"mklist" )
  lexical.delimiters ++= (": . < -> => + - * / ( ) [ ] { } , = ;" split ' ')
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

  def  program: PackratParser[ObSecExpr] = phrase(expr)//new Wrap("program",phrase(expr))
  lazy val expr : Parser[ObSecExpr] = {
    valExpr |||  varExpr ||| methodInvExpr | ifThenElse | letStarExpr | mkListExpr
  }

  lazy val mkListExpr : Parser[ObSecExpr] =
    ((MKLIST ~ LEFTPAREN) ~> repsep(expr,",")) <~ RIGHTPAREN ^^ (l => ListConstructorExpr(l))

  lazy val letStarExpr :PackratParser[ObSecExpr] =
    ((LET ~ LEFTBRACKET )~> (rep(defTypeDecl)~ rep(typeAliasDecl) ~ rep(localDecl))) ~ ((RIGHTBRACKET ~ IN) ~> expr) ^^
      {case defTypeDecls ~ tDecls ~ decls ~ expr => LetStarExpr(defTypeDecls++ tDecls ++ decls,expr)}

  lazy val typeAliasDecl : PackratParser[TypeAlias] =
    (("type" ~> ident <~ EQUALSSIGN) ~ objType) ^^ {case id ~ t => TypeAlias(id,t)}

  lazy val defTypeDecl : PackratParser[TypeDefinition] =
    "deftype" ~> ident ~ (LEFTBRACKET  ~> (methodList <~ RIGHTBRACKET)) ^^ {case tName ~ methodList => TypeDefinition(tName,methodList)}


  lazy val localDecl : PackratParser[LocalDeclaration] =
    localDecl11 | localDecl12

  lazy val localDecl11 : PackratParser[LocalDeclaration] =
    ((identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclaration(id,expr)}

  lazy val localDecl12 : PackratParser[LocalDeclaration] =
    (("val" ~> identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclaration(id,expr)}


  lazy val valExpr :  Parser[ObSecExpr] = objectExpr | objectExpr2 | primVal

  def varExpr : PackratParser[ObSecExpr] =  identifier ^^ (vName => Var(vName))


  lazy val methodInvExpr :PackratParser[ObSecExpr]={
    (expr <~ DOT) ~ (extendedIdentifier) ~ ((LEFTPAREN ~> repsep(expr, COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ args => MethodInv(e1,args,id)}
  }

  lazy val ifThenElse :PackratParser[ObSecExpr]= {
    (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {case c ~ e1 ~ e2 => IfExpr(c,e1,e2)}
  }

  lazy val  objectExpr: PackratParser[ObSecExpr] = {
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET) ^^
      {case self ~ stype ~ methodDefs => Obj(self,stype,methodDefs)}
  }
  lazy val  objectExpr2: PackratParser[ObSecExpr] = {
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
  lazy val stype : PackratParser[SType] = stypeX | publicTypeShortcut
  lazy val stypeX : PackratParser[SType] ={
    ((singleType <~ LESSTHAN) ~ labelType) ^^ {case t1 ~ t2  => t2 match {
      case LowLabel => SType(t1,t1)
      case HighLabel => SType(t1,ObjType.top)
      case _ => SType(t1,t2)
    } }
  }
  lazy val publicTypeShortcut: PackratParser[SType] ={
    singleType ^^ {t1  => SType(t1,t1)}
  }

  lazy val singleType : PackratParser[Type] ={
    objType  |  primType | varType
  }

  lazy val varType : PackratParser[Type] = identifier ^^ { id => TypeVar(id)}

  lazy val labelType : PackratParser[Type] ={
    objType | lowLabel | highLabel | primType | varType
  }
  lazy val primType : PackratParser[Type] = {
    intType | booleanType | stringListType | stringType
  }
  lazy val intType : PackratParser[Type] = INT ^^ {_=> IntType}
  lazy val stringType : PackratParser[Type] = STRING ^^ {_=> StringType}
  lazy val booleanType : PackratParser[Type] = BOOLEAN ^^ {_=> BooleanType}
  lazy val stringListType : PackratParser[Type] = STRLIST ^^ {_=> StringListType}


  lazy val lowLabel : PackratParser[Type]= LOW ^^ {_ => LowLabel}
  lazy val highLabel : PackratParser[Type]= HIGH ^^ {_ => HighLabel}

  lazy val primVal :  Parser[ObSecExpr] = stringLiteralExpr | integerExpr | boolExpr

  //lazy val stringLiteralExpr : PackratParser[ObSecExpr] = stringLit ^^ {s => StringExpr(s.substring(1,s.length-1))}
  lazy val stringLiteralExpr : Parser[ObSecExpr] = stringLit ^^ {s => StringExpr(s)}
  lazy val integerExpr : Parser[ObSecExpr] = (numericLit | negativeNumericLiteral) ^^ {s => IntExpr(s.toInt)}
  lazy val negativeNumericLiteral : Parser[String]= "-" ~> numericLit ^^ {s=> "-"+s}
  lazy val boolExpr : PackratParser[ObSecExpr] = ("true" | "false" ) ^^ {s => BooleanExpr(s == "true")}

  lazy val objType :  PackratParser[ObjType]= objType11 | objType13 | objType12

  lazy val objType11 :  PackratParser[ObjType]={
    (LEFTSBRACKET  ~> identifier) ~ (methodList <~ RIGHTSBRACKET) ^^ {case tVar ~ methodList => ObjType(TypeVar(tVar),methodList)}
  }
  lazy val objType12 :  PackratParser[ObjType]={
    ((LEFTBRACKET ~ OT) ~> identifier) ~ (methodList <~ RIGHTBRACKET) ^^ {case tVar ~ methodList => ObjType(TypeVar(tVar),methodList)}
  }
  lazy val objType13 :  PackratParser[ObjType]={
    LEFTSBRACKET  ~>  (methodList <~ RIGHTSBRACKET) ^^ {case methodList => ObjType(TypeVar("gen"),methodList)}
  }

  lazy val methodList : PackratParser[List[MethodDeclaration]]={
    rep(methodSignature)
  }
  lazy val methodSignature : PackratParser[MethodDeclaration]={
    ((LEFTBRACKET ~> extendedIdentifier) <~ COLON) ~ (rep(stype) <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName ~ argTypes ~ t2 => MethodDeclaration(mName,MType(argTypes,t2))}
  }

  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ObSecParserError, ObSecExpr] = {
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
  def parseType(string:String):Either[ObSecParserError, Type] = {
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
  def parseSType(string:String):Either[ObSecParserError, SType] = {
    //parseAll(stype,string) match {
    phrase(stype) (new lexical.Scanner(string)) match {
      case  x: NoSuccess => Left(ObSecParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

}

trait ObSecCompilationError
case class ObSecParserError(msg: String,pos:Position,offset:Int) extends ObSecCompilationError


