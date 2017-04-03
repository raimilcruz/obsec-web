

import ObSec.Parsing.ObSecParser
import ObSec.Static.AmadioCardelliSubtyping
import org.scalatest.FlatSpec

/**
  * Created by rcc on 4/2/2017.
  */
class AlphaEqSpec extends FlatSpec{
  "Alpha Eq 0" must "work" in{
    var subtyping =new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X}")
    var t2 = ObSecParser.parseType("{ot Y}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) => subtyping.alphaEq(t11,t22)
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Alpha Eq 1" must "work" in{
    var subtyping =new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : X<X -> Int<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : Y<Y -> Int<Int}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) => subtyping.recAlphaEq(Set(),t11,t22)
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Alpha Eq 2" must "fail" in{
    var subtyping =new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : X<X -> X<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : Y<Y -> Int<Int}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) => !subtyping.recAlphaEq(Set(),t11,t22)
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Alpha Eq 3" must "work" in{
    var subtyping =new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : {ot Z1 {boo : Z1<Z1 -> Int<Int}}<{ot Z2} -> X<X}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : {ot Z3 {boo : Z3<Z3 -> Int<Int}}<{ot Z4} -> Y<Y}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) => subtyping.recAlphaEq(Set(),t11,t22)
      case (_,_) => fail("types are not recognized!!")
    }
  }
}
