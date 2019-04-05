package scala.ObSecG

import Common._
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

class AlphaEqSpec extends FlatSpec{

  "Backward compatibility: Alpha Eq 0" must "work" in{
    var t1 = ObSecGParser.parseType("{ot X}")
    var t2 = ObSecGParser.parseType("{ot Y}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Backward compatibility: Alpha Eq 1" must "work" in{
    var t1 = ObSecGParser.parseType("{ot X {foo : X<X -> Int<Int}}")
    var t2 = ObSecGParser.parseType("{ot Y {foo : Y<Y -> Int<Int}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }

  "Backward compatibility: Alpha Eq 2" must "fail" in{
    var t1 = ObSecGParser.parseType("{ot X {foo : X<X -> X<Int}}")
    var t2 = ObSecGParser.parseType("{ot Y {foo : Y<Y -> Int<Int}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(!TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Backward compatibility: Alpha Eq 3" must "work" in{
    var t1 = ObSecGParser.parseType("{ot X {foo : {ot Z1 {boo : Z1<Z1 -> Int<Int}}<{ot Z2} -> X<X}}")
    var t2 = ObSecGParser.parseType("{ot Y {foo : {ot Z3 {boo : Z3<Z3 -> Int<Int}}<{ot Z4} -> Y<Y}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Alpha Eq with generic variables 1.1" must "work" in{
    var t1 = ObSecGParser.parseType("{ot a {m[X:a..Top]: a -> a<X}}")
    var t2 = ObSecGParser.parseType("{ot b {m[X:b..Top]: b -> b<X}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }
  "Alpha Eq with generic variables 1.2" must "fails" in{
    var t1 = ObSecGParser.parseType("{ot a {m[X:a..Top]: a -> a<X}}")
    var t2 = ObSecGParser.parseType("{ot b {m[Y:b..Top]: b -> b<b}}")
    (t1,t2) match {
      case (Right(t11),Right(t22)) =>
        val resolver = new ObSecGIdentifierResolver()
        assert(!TypeEquivalenceG.alphaEq(resolver.resolveType(t11),resolver.resolveType(t22)))
      case (_,_) => fail("types are not recognized!!")
    }
  }
}
