package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class AlphaEqSpec extends FlatSpec with Matchers with ElementServiceBaseSpec {

  "Subtyping T1 <: T1 v T2" must "work" in{
    val label1 = StringType.toObjType
    val label2 = StringType.toObjType

    assert(TypeEquivalenceG.alphaEq(label1,label2))
  }
}
