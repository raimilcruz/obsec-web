package scala.ObSecG

import Common.ErrorCollector
import ObSecG.Ast._
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}


class WellFormedGSpec extends FlatSpec with Matchers with BaseSpec {
  "Type [Obj(a){m<T> : T -> T}]" must "be well formed" in{
    val methodType =
      MTypeG(
        List(TypeVarSub("T",ObjectType.top)),
        List(
          ST(GV("T"),GV("T"))),
        ST(GV("T"),GV("T")))
    val objType = OT("a",List(MD("m",methodType)))

    val errorCollector = new ErrorCollector
    val wfChecker = new WellFormedCheckerG(errorCollector)
    assert(wfChecker.isWellFormed(objType))
  }
}
