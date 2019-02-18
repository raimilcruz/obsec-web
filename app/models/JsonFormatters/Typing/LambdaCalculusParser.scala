package models.JsonFormatters.Typing

import Common.judgment.examples.lambdacalculus._
import Common.{DebugPackratParsers, OffsetPositional, ParserError}

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.input.NoPosition


/**
  * Defining a parser for ObSec language
  */
object LambdaCalculusParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  lexical.reserved += ("lam", "Int" , "Bool")
  lexical.delimiters ++= (": . < > -> => + - * / ( ) [ ] { } , = ; <: .." split ' ')
/*

  val reserved: PackratParser[String] =
  {"if" | "then" | "else" | "true" | "false" | "Ref" | "Unit" | "ot" | "Int" | "String" | "Boolean" | "L" | "H" | "let" |"in" | "mklist" }
  val reserved2: List[String] =
  List("if" , "then" , "else" , "true" , "false" , "Ref" , "Unit" , "ot" , "Int" , "String" , "Boolean" , "L" , "H" , "let" ,"in" ,"mklist" )
*/

  //lazy val identifier: PackratParser[String] = elem("ident",x=> reserved2.contains(x.toString)) ^^ (x=>x.toString)
  lazy val identifier: PackratParser[String] = ident

  lazy val symIdentifier:PackratParser[String] =  rep("+"| "-"| "="|"<"|"/") ^^ (l => l.foldLeft("")((acc, a) => acc + a))

  /*lazy val extendedIdentifier : PackratParser[String] =
    (identifier | symIdentifier) ^^ {str => str}*/
  lazy val simpleIdentifier : PackratParser[String] = {
    ident ^^ (id => id)
  }
  lazy val extendedIdentifier : PackratParser[String] =  simpleIdentifier | symIdentifier

  //PARSERS FOR tokens
  def ARROW = "->"
  def RARROW = "=>"
  def COLON = ":"
  def LEFTPAREN = "("
  def RIGHTPAREN = ")"
  def DOT = "."
  def COMMMA =","
  def EQUALSSIGN = "="

  def INT = "Int"
  def STRING = "String"
  def BOOLEAN = "Bool"
  def STRLIST = "StrList"



  def  program: PackratParser[LambdaCalculusExpr] = phrase(expr)//new Wrap("program",phrase(expr))
  lazy val expr : Parser[LambdaCalculusExpr] = {
   varExpr ||| functionApp ||| lambda ||| primVal ||| parExpr
  }

  lazy val varExpr :  Parser[Var] = simpleIdentifier ^^ (s => Var(s))
  lazy val primVal :  Parser[LambdaCalculusExpr] = integerExpr
  lazy val integerExpr : Parser[Num] = numericLit  ^^ {s => Num(s.toInt)}
  lazy val parExpr :  Parser[LambdaCalculusExpr] = (("(" ~> expr) <~ ")") ^^ (e1 => ParExpr(e1))

  lazy val typeDecl : PackratParser[SimpleType] = primType | arrowType

  lazy val functionApp : PackratParser[App]={
    (expr ~ expr) ^^ {case e1 ~ e2 => App(e1,e2)}
  }

  lazy val lambda: PackratParser[Lambda] = {
    "lam" ~> (extendedIdentifier ~ (COLON ~> typeDecl)) ~ (DOT ~> expr) ^^
      { case aName ~ aType ~ bExpr => Lambda(aName,aType,bExpr)}
  }

  lazy val primType : PackratParser[SimpleType] = {
    intType
  }
  lazy val intType : PackratParser[SimpleType] = INT ^^ {_=> IntType}
  lazy val arrowType: PackratParser[Arrow]={
    (typeDecl ~ typeDecl) ^^
      {case  t1 ~ t2 =>  Arrow(t1,t2)}
  }


  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ParserError, LambdaCalculusExpr] = {
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
  def parseType(string:String):Either[ParserError, SimpleType] = {
    //parseAll(singleType,string) match {
    phrase(typeDecl) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }

}




