package scala.ObSec



import ObSec.Parsing.ObSecParser
import ObSec.Static.{TypeEquivalence, TypeSubst}
import org.scalatest.FlatSpec

/**
  * Created by rcc on 4/2/2017.
  */
class SubstitutionSpec extends FlatSpec {
  "Substitution test 1" must "work" in {
    val t1  = ObSecParser.parseType("x")
    val t2 = ObSecParser.parseType("{ot x {add: Int<Int -> x<x}}")
    (t1,t2) match{
      case (Right(type1),Right(type2)) =>
        assert(TypeEquivalence.alphaEq(TypeSubst.subst(type1,"x",type2),type2))
      case _ => fail("parser error")
    }
  }
  "Substitution test 2" must "work" in {
    val t1  = ObSecParser.parseType("{ot y {min : String<String -> {ot x}<{ot x}}" +
                                           "{foo : x<x -> y<y}}")
    val t2 = ObSecParser.parseType("{ot x {add: Int<Int -> x<x}}")

    val t3  = ObSecParser.parseType("{ot y {min : String<String -> {ot x}<{ot x}}" +
      "{foo : {ot x {add: Int<Int -> x<x}}<{ot x {add: Int<Int -> x<x}} -> y<y}}")
    (t1,t2,t3) match{
      case (Right(type1),Right(type2),Right(type3)) =>
        assert(TypeEquivalence.alphaEq(TypeSubst.subst(type1,"x",type2),type3))
      case _ => fail("parser error")
    }
  }
 /* "Substitution test 3" must "work" in {
    val t1  = ObSecParser.parseType("[{foo: ]" +
      "{foo : x<x -> y<y}}")
    val t2 = ObSecParser.parseType("{ot x {add: Int<Int -> x<x}}")

    val t3  = ObSecParser.parseType("{ot y {min : String<String -> {ot x}<{ot x}}" +
      "{foo : {ot x {add: Int<Int -> x<x}}<{ot x {add: Int<Int -> x<x}} -> y<y}}")
    (t1,t2,t3) match{
      case (Right(type1),Right(type2),Right(type3)) =>
        assert(TypeEquivalence.alphaEq(TypeSubst.subst(type1,"x",type2),type3))
      case _ => fail("parser error")
    }
  }*/
}
