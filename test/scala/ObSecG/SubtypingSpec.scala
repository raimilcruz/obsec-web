package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class SubtypingSpec extends FlatSpec with Matchers with BaseSpec {

  "Type [Obj(a){m<T> : T -> T}]" must "be well formed" in{

    val env = Environment.extend[TypeG](Environment.empty(),"T",ObjectType.top)
    var subtypingChecker = new AmadioCardelliSubtypingG
    assert(subtypingChecker.<::(env,GV("T"),GV("T")))
  }
}
