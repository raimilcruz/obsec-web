package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {

  "Type [Obj(a){m<T> : T -> T}]" must "be well formed" in{

    val env = Environment.empty[TypeVarBounds]()
              .extend("T",TypeVarBounds(Bottom,ObjectType.top))
    var judgements = new GObSecGJudgmentImpl(new ErrorCollector)
    var subtypingChecker = new AmadioCardelliSubtypingG(judgements,judgements.errorCollector)
    assert(subtypingChecker.<::(env,GV("T"),GV("T")))
  }
}
