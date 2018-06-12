package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {

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
    assert(subtypingChecker.<::(env,LabelVar("T"),IntType))
  }
}
