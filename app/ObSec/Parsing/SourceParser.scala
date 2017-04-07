package ObSec.Parsing

import ObSec.Ast.{ObSecExpr, _}
import ObSec.Parsing.ObSecParser.rep1

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}



/**
  * Defining a parser for ObSec language
  */
object ObSecParser extends JavaTokenParsers with PackratParsers with DebugPackratParsers{

  val reserved: PackratParser[String] =
  {"if" | "then" | "else" | "true" | "false" | "Ref" | "Unit | ot | Int | String | Boolean | L | H" | "let" |"in" | "mklist" }

  lazy val identifier: PackratParser[String] = not(reserved) ~> ident ^^ { str => str}

  lazy val symIdentifier =  """(\+|-|=|<|/)*""".r

  lazy val extendedIdentifier : PackratParser[String] =
    (identifier | symIdentifier) ^^ {str => str}

  //PARSERS FOR tokens
  def OT = "ot"
  def ARROW = "->"
  def RARROW = "=>"
  def COLON = ":"
  def LEFTBRACKET = "{"
  def RIGHTBRACKET = "}"
  def LEFTPAREN = "("
  def RIGHTPAREN = ")"
  def LESSTHAN = "<"
  def DOT = "."
  def COMMMA =","
  def EQUALSSIGN = "="
  def IF = "if"
  def THEN = "then"
  def ELSE = "else"
  def MKLIST = "mklist"

  def INT = "Int"
  def STRING = "String"
  def BOOLEAN = "Bool"
  def STRLIST = "StrList"

  def LET ="let"
  def IN ="in"

  def LOW ="L"
  def HIGH ="H"

  def operPlus = "+"
  def operMinus = "-"
  def operCompare = "=="

  lazy val operator : PackratParser[String] = operPlus | operMinus | operCompare

  def  program: PackratParser[ObSecExpr] = new Wrap("program",phrase(expr))
  lazy val expr : PackratParser[ObSecExpr] = {
    valExpr |||  varExpr ||| methodInvExpr | ifThenElse | letStarExpr | mkListExpr
  }

  lazy val mkListExpr :PackratParser[ObSecExpr] =
    ((MKLIST ~ LEFTPAREN) ~> repsep(expr,",")) <~ RIGHTPAREN ^^ {case l => ListConstructorExpr(l)}

  lazy val letStarExpr :PackratParser[ObSecExpr] =
    ((LET ~ LEFTBRACKET )~> rep(localDecl)) ~ ((RIGHTBRACKET ~ IN) ~> expr) ^^ {case decls ~ expr => LetStarExpr(decls,expr)}

  lazy val localDecl : PackratParser[LocalDeclaration] =
    ((identifier <~ EQUALSSIGN) ~ expr) ^^ {case id ~ expr => LocalDeclaration(id,expr)}


  lazy val valExpr : PackratParser[ObSecExpr] = objectExpr | primVal

  def varExpr : PackratParser[ObSecExpr] =  identifier ^^ { case vName => Var(vName)}


  lazy val methodInvExpr :PackratParser[ObSecExpr]={
    (expr <~ DOT) ~ (identifier | operator) ~ ((LEFTPAREN ~> repsep(expr, COMMMA)) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ args => MethodInv(e1,args,id)}
  }

  lazy val ifThenElse :PackratParser[ObSecExpr]= {
    (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {case c ~ e1 ~ e2 => IfExpr(c,e1,e2)}
  }

  lazy val  objectExpr: PackratParser[ObSecExpr] = {
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ stype ~ ((RARROW ~> methodDefs) <~ RIGHTBRACKET) ^^
      {case self ~ stype ~ methodDefs => Obj(self,stype,methodDefs)}
  }
  lazy val methodDefs : PackratParser[List[MethodDef]]={
    rep(methodDef)
  }
  lazy val methodDef : PackratParser[MethodDef] = {
    LEFTBRACKET ~> (extendedIdentifier ~ rep(identifier)) ~ ((EQUALSSIGN ~> expr ) <~ RIGHTBRACKET) ^^
      { case mName ~ args ~ expr => MethodDef(mName,args,expr)}
  }
  lazy val stype : PackratParser[SType] ={
    ((singleType <~ LESSTHAN) ~ labelType) ^^ {case t1 ~ t2  => t2 match {
      case LowLabel => SType(t1,t1)
      case HighLabel => SType(t1,ObjType.top)
      case _ => SType(t1,t2)
    } }
  }
  lazy val singleType : PackratParser[Type] ={
    objType |  primType | varType
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

  lazy val primVal : PackratParser[ObSecExpr] = stringLiteralExpr | integerExpr | boolExpr

  lazy val stringLiteralExpr : PackratParser[ObSecExpr] = stringLiteral ^^ {s => StringExpr(s.substring(1,s.length-1))}
  lazy val integerExpr : PackratParser[ObSecExpr] = wholeNumber ^^ {s => IntExpr(s.toInt)}
  lazy val boolExpr : PackratParser[ObSecExpr] = ("true" | "false" ) ^^ {s => BooleanExpr(s == "true")}

  lazy val objType :  PackratParser[ObjType]={
    ((LEFTBRACKET ~ OT) ~> identifier) ~ (methodList <~ RIGHTBRACKET) ^^ {case tVar ~ methodList => ObjType(TypeVar(tVar),methodList)}
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
    parseAll(program,string) match {
      case NoSuccess(msg, next) => Left(ObSecParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseType(string:String):Either[ObSecParserError, Type] = {
    parseAll(singleType,string) match {
      case NoSuccess(msg, next) => Left(ObSecParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
  /**
    * Api method: Parses a type from a source
    * @param string The type syntax
    * @return The expression representing the type
    */
  def parseSType(string:String):Either[ObSecParserError, SType] = {
    parseAll(stype,string) match {
      case NoSuccess(msg, next) => Left(ObSecParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

}

trait ObSecCompilationError
case class ObSecParserError(msg: String) extends ObSecCompilationError


