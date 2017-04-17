package scala

import ObSec.Parsing.ObSecParser
import ObSec.Static._
import org.scalatest.FlatSpec

/**
  * Created by racruz on 04-04-2017.
  */
class WellFormedSpec extends FlatSpec{
  val wellFormedChecker = new WellFormedChecker(new ErrorCollector)
  "WF for ST" must "fail" in {
    val t = ObSecParser.parseSType("{ot x}<{ot y {f : Int<Int -> Int<Int}}")
    t match{
      case Right(tt) =>
        assert(!wellFormedChecker.isWellFormed(tt))
        println(wellFormedChecker.errorCollector.errors)
      case _ => fail("parsing error")
    }
  }
  "WF for ST" must "work" in {
    val t = ObSecParser.parseSType("{ot y {f : Int<Int -> Int<Int}{g : String<String -> Int<Int}}<{ot x {f : Int<Int -> Int<Int}}")
    t match{
      case Right(tt) =>
        assert(wellFormedChecker.isWellFormed(tt))
        println(wellFormedChecker.errorCollector.errors)
      case _ => fail("parsing error")
    }
  }

  "WF for ST" must "work 2" in {
    val t = ObSecParser.parseSType("String<{ot x {== : String<String -> Bool<Bool}}")
    t match{
      case Right(tt) =>
        val r = wellFormedChecker.isWellFormed(tt)
        println(wellFormedChecker.errorCollector.errors)
        assert(r)
      case _ => fail("parsing error")
    }
  }
  "WF for String" must "fail 2" in {
    val t = ObSecParser.parseSType("String<{ot y {isEmpty : -> Bool<L}{head : -> String<L}{tail : -> y<y}}")
    t match{
      case Right(tt) =>
        val r = !wellFormedChecker.isWellFormed(tt)
        println(wellFormedChecker.errorCollector.errors)
        assert(r)
      case _ => fail("parsing error")
    }
  }
  "WF for ST" must "work type variables" in {
    val t = ObSecParser.parseSType("{ot x {add : -> x<x}}<L")
    t match{
      case Right(tt) =>
        val r = wellFormedChecker.isWellFormed(tt)
        println(wellFormedChecker.errorCollector.errors)
        assert(r)
      case _ => fail("parsing error")
    }
  }


  "WF for ST" must "fail 2" in {
    val t = ObSecParser.parseSType("{ot y {f : Int<Int -> Int<Int}{g : String<Int -> Int<Int}}<{ot x {f : Int<Int -> Int<Int}}")
    t match{
      case Right(tt) => assert(!wellFormedChecker.isWellFormed(tt))
      case _ => fail("parsing error")
    }
  }
  "WF for ST" must "fail 3" in {
    val t = ObSecParser.parseSType("{ot X {login : String<{ot y {isEmpty : -> Bool<L}{head : -> String<L}{tail : -> y<y}} String<L -> Int<L}}<{ot x}")
    t match{
      case Right(tt) => assert(!wellFormedChecker.isWellFormed(tt))
      case _ => fail("parsing error")
    }
  }

  "WF for type" must "work 3" in {
    val t = ObSecParser.parseType("{ot X {m : Int<Int -> X<{ot y {m: Int<Int -> X<y}}}}")
    t match{
      case Right(tt) => assert(wellFormedChecker.isWellFormed(tt))
      case _ => fail("parsing error")
    }

  }
  "WF for type with repeated method" must "fail" in {
    val t = ObSecParser.parseType("[{m : -> Int<Int}{m : -> Int<Int}]")
    t match{
      case Right(tt) =>
          assert(!wellFormedChecker.isWellFormed(tt))
      case _ => fail("parsing error")
    }

  }

}
