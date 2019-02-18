package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {


  /*"Subtyping T1 <: T1 v T2" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    //Int <: Int v String
    assert(subtypingChecker.<::(env,IntType,UnionLabel(IntType,StringType))== SubtypingSuccess)
  }
  "Subtyping T2 <: T1 v T2" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,StringType,UnionLabel(IntType,StringType)) == SubtypingSuccess)
  }

  "Subtyping T1 v T2 <: T where T1 <: T and T2 <:T" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,UnionLabel(StringType,StringType),StringType)== SubtypingSuccess)
  }
  "Subtyping T1 v T2 <: T where T1 <: T and T2 </:T" must "not work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,UnionLabel(StringType,IntType),StringType).isInstanceOf[SubtypingFail])
  }
  "Subtyping T1 v T2 <: T where T1 </: T and T2 <:T" must "not work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,UnionLabel(IntType,StringType),StringType).isInstanceOf[SubtypingFail])
  }

  "Subtyping between union type and label variable" must "work" in{

    val left  =  UnionLabel(IntType,LabelVar("T"))
    val rigth  =  LabelVar("T1")

    val env = Environment.empty[TypeVarBounds]().
      extend("T",TypeVarBounds(IntType,OT("X2",List()))).
      extend("T1",TypeVarBounds(LabelVar("T"),OT("X2",List())))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    //T: Int .. Top, T1: T ..Top
    assert(subtypingChecker.<::(env,left,rigth) == SubtypingSuccess)
  }*/
  "Subtyping between Int and type variable bound" must "work" in{

    val left  =  IntADT
    val env = Environment.empty[TypeVarBounds]().
      extend("T",TypeVarBounds(IntADT,OT("X2",List()))).
      extend("T1",TypeVarBounds(LabelVar("T"),OT("X2",List())))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    //T: Int .. Top, T1: T ..Top
    //assert(subtypingChecker.<::(env,left,ObjectType.top))
    assert(subtypingChecker.<::(env,IntADT,LabelVar("T")) == SubtypingSuccess)
  }

  "String <: [==[T super String]: String<L -> String<L]" must "work" in{

    val stringEq  = ObSecGParser.parseType("[{==[low T super String]: String<T -> Bool<(Bool,T)}]")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType) == SubtypingSuccess)
    }
  }
  "String </: Bad StringEq" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {== : String<String -> Bool<Bool}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType).isInstanceOf[SubtypingFail])
    }
  }
  "String <: StringEq, where StringEq where the result does not use the label" must "not work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {==[low T : String .. String] : String<String -> Bool<Bool}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType).isInstanceOf[SubtypingFail])
    }
  }

  "String <: StringEq, where StringEq where the result uses the label" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {==[low T : String .. String] : String<String -> Bool<(Bool,T)}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType) == SubtypingSuccess)
    }
  }

  "String <: StringEq, where StringEq where the result does not use a low label" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {==[T : String .. String] : String<String -> Bool<(Bool,T)}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringADT,stringEqType) == SubtypingSuccess)
    }
  }

  "StringList <: StringGenList[Top]" must "work" in{
      var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
      var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

      assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringListType,StringGListType(ObjectType.top)) == SubtypingSuccess)

  }
  "StringGenList[Top] <: StringGenList[String]" must "not work" in{
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringGListType(ObjectType.top),StringGListType(StringADT)) != SubtypingSuccess)

  }
}
