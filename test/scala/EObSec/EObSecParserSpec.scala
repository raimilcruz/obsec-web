package scala.EObSec

import ObSecE.Ast._
import ObSecE.Parsing.EObSecParser
import org.scalatest.{FlatSpec, Matchers}

class EObSecParserSpec extends FlatSpec with Matchers with BaseSpec {
  "Existential types" must "be recognized" in {
    val existentialType = "exists X super Int,Y super Int.[{m : Int -> Int}]"
    val res = EObSecParser.parseLabelType(existentialType)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
  "Security type with existential types " must "be recognized" in {
    val securityType = "{ot x} with Int as exists X super Int,Y super Int.[{m : Int -> Int}]"
    val res = EObSecParser.parseSType(securityType)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
  "Existential security type with no existential types " must "fail" in {
    val securityType = "{ot x} with Int as Int"
    val res = EObSecParser.parseSType(securityType)
    res match{
      case Left(x)=> assert(true)
      case Right(y)=> fail("Parser should not recognize it!")
    }
  }
  "Existential security type with global secrets" must "be recognized" in {
    val existentialType = "exists glob X super Int, glob Y super Int.[{m : Int -> Int}]"
    val res = EObSecParser.parseLabelType(existentialType)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
  "Existential security type mixing global and local secrets" must "be recognized" in {
    val existentialType = "exists glob X super Int, Y super Int.[{m : Int -> Int}]"
    val res = EObSecParser.parseLabelType(existentialType)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
  "Alias to existential type" must "be recognized" in {
    val program = "let{ \n  type A = exists X super Int.[{m : -> Int}]\n} \nin 1"
    val res = EObSecParser(program)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
}
