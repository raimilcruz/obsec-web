package models.JsonFormatters.Typing


import Common.{DebugPackratParsers, ParserError}

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}


object EnvironmentParser extends StandardTokenParsers with PackratParsers with ImplicitConversions with DebugPackratParsers{
  //lexical.reserved += ("lam", "Int" , "Bool")
  lexical.delimiters ++= (": . < > -> => + - * / ( ) [ ] { } , = ; <: .." split ' ')

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
  def COLON = ":"

  def EQUALSSIGN = "="

  def  program: PackratParser[List[(String,String)]] = phrase(env)//new Wrap("program",phrase(expr))
  lazy val env: Parser[List[(String,String)]] =  repsep(assocPair, ",")


  lazy val assocPair :  Parser[(String,String)] = (identifier ~ (COLON ~> identifier)) ^^ {case s1 ~ s2 => (s1,s2)}


  /**
    * Api method: Builds an AST from a source
    * @param string The program source
    * @return An AST
    */
  def apply(string: String): Either[ParserError, List[(String,String)]] = {
    //parseAll(program,string) match {
    phrase(env) (new lexical.Scanner(string)) match {
      case x: NoSuccess => Left(ParserError("["+x.next.pos+"] failure: "+x.msg,x.next.pos,x.next.rest.pos,x.next.offset))
      case Success(result, next) => Right(result)
    }
  }
}




