package scala.EObSec

import ObSecE.Ast._
import ObSecE.Parsing.EObSecParser
import org.scalatest.{FlatSpec, Matchers}

class SoundGlobalPoliciesSpec extends FlatSpec with Matchers with BaseSpec {
  "Avg example" must  "be sound" in {
    val exitentialFacet = "exists glob X super Int, glob Y super Int. [{avg: Int<X Int<Y -> Int<Int}]"
    val res = EObSecParser.parseLabelType(exitentialFacet)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
}
