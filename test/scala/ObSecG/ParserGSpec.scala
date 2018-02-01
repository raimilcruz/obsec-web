package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Parsing.ObSecGParser
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class ParserGSpec extends FlatSpec with Matchers{
  "parser" should "work with {self : {ot x }<{ot x} => }" in {
    val res = ObSecGParser("{self : {ot x }<{ot x} => }")
    assert(res == Right(Obj("self", STypeG(ObjectType("x", List()), ObjectType("x", List())), List())))
  }
}
