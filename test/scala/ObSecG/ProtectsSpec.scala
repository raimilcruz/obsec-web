package scala.ObSecG

import Common.{Environment, ErrorCollector}
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}


class ProtectsSpec extends FlatSpec with Matchers with BaseSpec {
  "Interface for Int type " must "be sound" in{
    val intInterface = IntType.toObjType

    val errorCollector = new ErrorCollector
    val judgments = new GObSecGJudgmentImpl(errorCollector)

    val protectsJudgement = new ProtectsJudgement(judgments,errorCollector)
    assert(protectsJudgement.sound(intInterface) == SoundSuccess)

  }
  "Interface for String type " must "be sound" in{
    val intInterface = IntType.toObjType

    val errorCollector = new ErrorCollector
    val judgments = new GObSecGJudgmentImpl(errorCollector)

    val protectsJudgement = new ProtectsJudgement(judgments,errorCollector)
    assert(protectsJudgement.sound(intInterface) == SoundSuccess)

  }
  "Interface for Bool type " must "be sound" in{
    val intInterface = IntType.toObjType

    val errorCollector = new ErrorCollector
    val judgments = new GObSecGJudgmentImpl(errorCollector)

    val protectsJudgement = new ProtectsJudgement(judgments,errorCollector)
    assert(protectsJudgement.sound(intInterface) == SoundSuccess)

  }
  "Interface [{== : Int<Top -> Bool<Bool}]" must "be unsound" in{
    val typeAnnotation = ObSecGParser.parseType("[{== : Int<Top -> Bool<Bool}]")
    typeAnnotation match{
      case Right(parsedInterface) =>
        val resolver = new ObSecGIdentifierResolver
        val typeInterface  = resolver.resolveType(parsedInterface).asInstanceOf[ObjectType]

        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)

        val protectsJudgement = new ProtectsJudgement(judgments,errorCollector)
        assert(protectsJudgement.sound(typeInterface) != SoundSuccess)
      case Left(error) => fail(error.msg)
    }
  }
  "Interface [{== : Int<Int -> Bool<Bool}]" must "be sound" in{
    val typeAnnotation = ObSecGParser.parseType("[{== : Int<Int -> Bool<Bool}]")
    typeAnnotation match{
      case Right(parsedInterface) =>
        val resolver = new ObSecGIdentifierResolver
        val typeInterface  = resolver.resolveType(parsedInterface).asInstanceOf[ObjectType]

        val errorCollector = new ErrorCollector
        val judgments = new GObSecGJudgmentImpl(errorCollector)

        val protectsJudgement = new ProtectsJudgement(judgments,errorCollector)
        assert(protectsJudgement.sound(typeInterface) != SoundSuccess)
      case Left(error) => fail(error.msg)
    }
  }
}
