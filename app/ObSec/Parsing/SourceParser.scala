package ObSec.Parsing

import ObSec.Ast.{ObSecExpr, _}

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}



/**
  * Defining a parser for ObSec language
  */
object ObSecParser extends JavaTokenParsers with PackratParsers with DebugPackratParsers{

  val reserved: PackratParser[String] =
  {"if" | "then" | "else" | "true" | "false" | "Ref" | "Unit | ot | Int | String | Boolean | L | H"}

  lazy val identifier: PackratParser[String] = not(reserved) ~> ident ^^ { str => str}

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
  def EQUALSSIGN = "="
  def IF = "if"
  def THEN = "then"
  def ELSE = "else"

  def INT = "Int"
  def STRING = "String"
  def BOOLEAN = "Boolean"

  def LOW ="L"
  def HIGH ="H"

  def operPlus = "+"
  def operMinus = "-"
  def operCompare = "=="

  lazy val operator : PackratParser[String] = operPlus | operMinus | operCompare

  def  program: PackratParser[ObSecExpr] = new Wrap("program",phrase(expr))
  lazy val expr : PackratParser[ObSecExpr] = {
    valExpr |||  varExpr ||| methodInvExpr | ifThenElse
  }

  lazy val valExpr : PackratParser[ObSecExpr] = objectExpr | primVal

  def varExpr : PackratParser[ObSecExpr] =  identifier ^^ { case vName => Var(vName)}


  lazy val methodInvExpr :PackratParser[ObSecExpr]={
    (expr <~ DOT) ~ (identifier | operator) ~ ((LEFTPAREN ~> expr) <~ RIGHTPAREN)^^
      {case e1 ~ id ~ e2 => MethodInv(e1,e2,id)}
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
    LEFTBRACKET ~> (identifier ~ identifier) ~ ((EQUALSSIGN ~> expr ) <~ RIGHTBRACKET) ^^
      { case mName ~ vName ~ expr => MethodDef(mName,vName,expr)}
  }
  lazy val stype : PackratParser[SType] ={
    ((singleType <~ LESSTHAN) ~ labelType) ^^ {case t1 ~ t2  => SType(t1,t2)}
  }
  lazy val singleType : PackratParser[Type] ={
    objType |  primType | varType
  }

  lazy val varType : PackratParser[Type] = identifier ^^ { id => TypeVar(id)}

  lazy val labelType : PackratParser[Type] ={
    objType | lowLabel | highLabel | primType | varType
  }
  lazy val primType : PackratParser[Type] = {
    intType | booleanType | stringType
  }
  lazy val intType : PackratParser[Type] = INT ^^ {_=> IntType}
  lazy val stringType : PackratParser[Type] = STRING ^^ {_=> StringType}
  lazy val booleanType : PackratParser[Type] = BOOLEAN ^^ {_=> BooleanType}


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
    ((LEFTBRACKET ~> identifier) <~ COLON) ~ (stype <~ ARROW) ~ (stype <~ RIGHTBRACKET) ^^
      {case  mName ~ t1 ~ t2 => MethodDeclaration(mName,MType(t1,t2))}
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

}

trait ObSecCompilationError
case class ObSecParserError(msg: String) extends ObSecCompilationError


