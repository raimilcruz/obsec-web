package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {

  "Subtyping between Int and X:Int..Top" must "work" in{
    val env = Environment.empty[TypeVarBounds]().
      extend("X",TypeVarBounds(IntADT,ObjectType.top))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,IntADT,LabelVar("X")) == SubtypingSuccess)
  }
  "Subtyping between X:Int..Top and Top" must "work" in{
    val env = Environment.empty[TypeVarBounds]().
      extend("X",TypeVarBounds(IntADT,ObjectType.top))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,LabelVar("X"),ObjectType.top) == SubtypingSuccess)
  }
  "Subtyping between X:Int..Top and Y:Top.. Top" must "work" in{
    val env = Environment.empty[TypeVarBounds]().
      extend("X",TypeVarBounds(IntADT,ObjectType.top))
      .extend("Y",TypeVarBounds(ObjectType.top,ObjectType.top))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,LabelVar("X"),LabelVar("Y")) == SubtypingSuccess)
  }

  "String <: [== : String<I -> String<I]" must "work" in{

    val stringEq  = ObSecGParser.parseType("[{==: String<I -> Bool<I}]")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType) == SubtypingSuccess)
    }
  }
  "String <: [{== : String<String -> Bool<Bool}]" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {== : String<String -> Bool<Bool}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType) == SubtypingSuccess)
    }
  }

/*
  "StringList <: StringGenList[Top]" must "work" in{
      var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
      var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

      assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringListType,StringGListType(ObjectType.top)) == SubtypingSuccess)

  }*/
  "StringGenList[Top] <: StringGenList[String]" must "not work" in{
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringGListType(ObjectType.top),StringGListType(StringADT)) != SubtypingSuccess)

  }
}
