package scala.ObSec



import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Static.AmadioCardelliSubtyping
import org.scalatest.FlatSpec

/**
  * Created by rcc on 4/2/2017.
  */
class SubtypingSpec extends FlatSpec {

  "Subtyping " must "work for primitive types" in {
    var subtyping = new AmadioCardelliSubtyping
    assert(subtyping.<::(IntType, IntType) &&
      subtyping.<::(BooleanType, BooleanType) &&
      subtyping.<::(StringType, StringType))
  }

  "Subtyping is not dummy" must "with primitive types" in {
    var subtyping = new AmadioCardelliSubtyping
    assert(!subtyping.<::(StringType, IntType) &&
      !subtyping.<::(StringType, BooleanType) &&
      !subtyping.<::(IntType, StringType))
  }

  "Subtyping for security type" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    assert(subtyping.<::(SType(IntType, IntType), SType(IntType, IntType)))
  }

  "Subtyping for simple type" must "fail" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot x}")
    var t2 = ObSecParser.parseType("Int")

    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(!subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Subtyping for security type" must "fail" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseSType("Int<{ot x}")
    var t2 = ObSecParser.parseSType("Int<Int")

    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(!subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }

  "Top " must "be top :-)" in {
    var subtyping = new AmadioCardelliSubtyping
    assert(subtyping.<::(IntType, ObjType.top) &&
      subtyping.<::(BooleanType, ObjType.top) &&
      subtyping.<::(StringType, ObjType.top) &&
      subtyping.<::(StringType, ObjType(TypeVar("Y"), List())) &&
      subtyping.<::(StringType, ObjType(TypeVar("Z"), List())))
  }
  "Wide record type subtyping" must "work (no type variables)" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {add : Int<Int -> Int<Int}{minus : Int<Int -> Int<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {add : Int<Int -> Int<Int}}")

    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Deep record type subtyping" must "work (no type variables)" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {add : Int<Int -> Int<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {add : Int<Int -> {ot X}<{ot X}}}")

    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Deep record type subtyping" must "fail with covariant codomains" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {add : Int<Int -> Int<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {add : {ot X}<{ot X} -> Int<Int}}")

    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(!subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Spec for int type" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {+ : Int<Int -> Int<Int}{-: Int<Int -> Int<Int}}")

    t1 match {
      case Right(t11) => assert(subtyping.<::(IntType, t11))
      case _ => fail("types are not recognized!!")
    }
  }
  "Spec for string type" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {== : String<String -> Bool<Bool}}")

    t1 match {
      case Right(t11) => assert(subtyping.<::(StringType, t11))
      case _ => fail("types are not recognized!!")
    }
  }

  "Subtyping recursive type 0" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : Int<Int -> X<X}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : Int<Int -> Y<Y}}")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Subtyping recursive type 1" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : {ot X}<{ot X} -> X<X}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : Int<Int -> Y<Y}}")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Subtyping recursive type 2" must "fail" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot X {foo : X<X -> Int<Int}}")
    var t2 = ObSecParser.parseType("{ot Y {foo : Y<Y -> {ot X}<{ot X}}}")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(!subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Subtyping recursive type 3" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    var t1 = ObSecParser.parseType("{ot x {foo : {ot z1}<{ot z1} -> x<x}}")
    var t2 = ObSecParser.parseType("{ot y {foo : Int<Int -> " +
      "{ot X {foo : Int<Int -> y<y}}<{ot X {foo : Int<Int -> y<y}}}}")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  /*"Subtyping case from A&C " must "work 1" in {
    var subtyping = new AmadioCardelliSubtyping
    val t1 = ObSecParser.parseType("{ot x {f : x<x -> {ot y {f: x<x -> x<x}}<L }}")
    val t2 = ObSecParser.parseType("{ot y {f : {ot z {foo: y<y -> y<y}}<L -> y<y}}")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22))
      case (_, _) => fail("types are not recognized!!")
    }
  }*/
  "Subtyping specification for StrList" must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    val t1 = ObSecParser.parseType("{ot l {isEmpty : -> Bool<L}{head : -> String<L}{tail : -> l<l}}")
    val t2 = ObSecParser.parseType("StrList")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22)&&subtyping.<::(t22, t11))
      case (_, _) => fail("types are not recognized!!")
    }
  }
  "Subtyping list policy " must "work" in {
    var subtyping = new AmadioCardelliSubtyping
    val t1 = ObSecParser.parseType("{ot l {isEmpty : -> Bool<L}{head : -> String<L}{tail : -> l<l}}")
    val t2 = ObSecParser.parseType("StrList")
    (t1, t2) match {
      case (Right(t11), Right(t22)) => assert(subtyping.<::(t11, t22)&&subtyping.<::(t22, t11))
      case (_, _) => fail("types are not recognized!!")
    }
  }
}
