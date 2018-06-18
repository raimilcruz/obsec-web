package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {

  "Subtyping T1 <: T1 v T2" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    //Int <: Int v String
    assert(subtypingChecker.<::(env,IntType,UnionLabel(IntType,StringType)))
  }
  "Subtyping T2 <: T1 v T2" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,StringType,UnionLabel(IntType,StringType)))
  }

  "Subtyping T1 v T2 <: T where T1 <: T and T2 <:T" must "work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(subtypingChecker.<::(env,UnionLabel(StringType,StringType),StringType))
  }
  "Subtyping T1 v T2 <: T where T1 <: T and T2 </:T" must "not work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(!subtypingChecker.<::(env,UnionLabel(StringType,IntType),StringType))
  }
  "Subtyping T1 v T2 <: T where T1 </: T and T2 <:T" must "not work" in{

    val env = Environment.empty[TypeVarBounds]()
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

    assert(!subtypingChecker.<::(env,UnionLabel(IntType,StringType),StringType))
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
    assert(subtypingChecker.<::(env,left,rigth))
  }
  "Subtyping between Int and type variable bound" must "work" in{

    val left  =  IntType
    val env = Environment.empty[TypeVarBounds]().
      extend("T",TypeVarBounds(IntType,OT("X2",List()))).
      extend("T1",TypeVarBounds(LabelVar("T"),OT("X2",List())))

    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    //T: Int .. Top, T1: T ..Top
    //assert(subtypingChecker.<::(env,left,ObjectType.top))
    assert(subtypingChecker.<::(env,IntType,LabelVar("T")))
  }

  "String <: [==: String<L -> String<L]" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {==[T super String]: String<T -> Bool<(Bool,T)}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringType,stringEqType))
    }
  }
  "String </: Bad StringEq" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {== : String<String -> Bool<Bool}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(!subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringType,stringEqType))
    }
  }
  "String <: StringEq redudant" must "work" in{

    val stringEq  = ObSecGParser.parseType("{ot rr {==[T : String .. String] : String<T -> Bool<Bool}}")
    stringEq match{
      case Right(typ) =>
        val stringEqType = new ObSecGIdentifierResolver().resolveType(typeAnnotation = typ)
        var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
        var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)

        assert(!subtypingChecker.<::(Environment.empty[TypeVarBounds](),StringType,stringEqType))
    }
  }
}
