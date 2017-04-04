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

  "WF for ST" must "fail 2" in {
    val t = ObSecParser.parseSType("{ot y {f : Int<Int -> Int<Int}{g : String<Int -> Int<Int}}<{ot x {f : Int<Int -> Int<Int}}")
    t match{
      case Right(tt) => assert(wellFormedChecker.isWellFormed(tt))
      case _ => fail("parsing error")
    }
  }
}
