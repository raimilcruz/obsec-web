package scala.ObSecG

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}


class WellFormedGSpec extends FlatSpec with Matchers with BaseSpec {
  "Type String<[{== : String<I -> Bool<U}]" must "be well formed" in{
    val annotatedType = ObSecGParser.parseSType("String<[{== : String<I -> Bool<I}]")
    annotatedType match{
      case Right(typeAnnotation) => {
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveSType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      }
      case Left(e) => fail(s"Syntax error: $e")
    }
  }
  "Type String<[{== : String -> Bool}]" must "be well formed" in{
    val annotatedType = ObSecGParser.parseSType("String<[{== : String -> Bool}]")
    annotatedType match{
      case Right(typeAnnotation) => {
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveSType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      }
      case Left(e) => fail(s"Syntax error: $e")
    }
  }
  "Type String<[{== : String<H -> Bool}]" must "is not  well formed" in{
    val annotatedType = ObSecGParser.parseSType("String<[{== : String<H -> Bool}]")
    annotatedType match{
      case Right(typeAnnotation) =>
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveSType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(!judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      case Left(e) => fail(s"Syntax error: $e")
    }
  }
  "Type {== : String<I -> Bool}]" must "is not well formed" in{
    val annotatedType = ObSecGParser.parseType("[{== : String<I -> Bool}]")
    annotatedType match{
      case Right(typeAnnotation) =>
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(!judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      case Left(e) => fail(s"Syntax error: $e")
    }
  }
  "Type String<[ot a {first: -> String<a}]" must "is well formed" in{
    val annotatedType = ObSecGParser.parseSType("String<[a {first: -> String<a}]")
    annotatedType match{
      case Right(typeAnnotation) =>
        val resolver = new ObSecGIdentifierResolver
        val resolvedType =  resolver.resolveSType(typeAnnotation)
        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)
        assert(judgments.isWellFormed(Environment.empty[TypeVarBounds](),resolvedType))
      case Left(e) => fail(s"Syntax error: $e")
    }
  }
}
