package scala.ObSecG

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}


class WellFormedGSpec extends FlatSpec with Matchers with BaseSpec {
  "Type {login: String<[{== : String -> Bool}] String -> Int}" must "be not well formed" in{
    val annotatedType = ObSecGParser.parseType("[{login: String<[{== : String -> Bool}] String -> Int}]")
    annotatedType match{
      case Right(typeAnnotation) => {
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(!judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      }
      case _ => fail("Syntax error")
    }
  }

  "Low label variable" must "work 1" in{
    val annotatedType = ObSecGParser.parseType("[{==[low T super String] : String<T -> Bool<(Bool,T)}]")
    annotatedType match{
      case Right(typeAnnotation) =>
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      case _ => fail("Syntax error")
    }
  }
}
