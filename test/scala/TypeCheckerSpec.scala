

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Static.{AmadioCardelliSubtyping, TypeChecker, TypeError}
import org.scalatest.FlatSpec

/**
  * Created by racruz on 31-03-2017.
  */
class TypeCheckerSpec extends FlatSpec {
  "type-checker" should "work with empty object" in {
    var expr = ObSecParser("{s : {ot X}<{ot X} => }")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(ObjType(TypeVar("X"), List()), ObjType(TypeVar("X"), List()))
        assert(res == expected)
      }
    }
  }

  "type-checker" should "work with objects with methods" in {
    var expr = ObSecParser("{s : {ot X {add : Int<Int -> Int<Int}}<{ot X} => {add x = x.+(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(
          ObjType(
            TypeVar("X"),
            List(MethodDeclaration(
              "add",
              MType(List(SType(IntType, IntType)), SType(IntType, IntType))))),
          ObjType(TypeVar("X"), List()))

        assert(res == expected)
      }
    }
  }
  it should "throw TypeError if an object has repeated methods" in {
    var expr = ObSecParser("{s : {ot X {add : Int<Int -> Int<Int}}<{ot X} => {add x = x.+(x)}{add x = x.-(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeError] {
          TypeChecker(ast)
        }
      }
    }
  }
  it should "throw TypeError if a method is in object type, but is not in the definition" in {
    var expr = ObSecParser("{s : {ot X {minus : Int<Int -> Int<Int}}<{ot X} => {add x = x.-(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeError] {
          TypeChecker(ast)
        }
      }
    }
  }
  it should "throw TypeError if a method is defined, but is not in object type" in {
    var expr = ObSecParser("{s : {ot X }<{ot X} => {add x = x}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeError] {
          TypeChecker(ast)
        }
      }
    }
  }
  "type checker " should "work with if expressions" in {
    var expr = ObSecParser("if true then 1 else 2")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(IntType,IntType)
        assert(res == expected)
      }
    }
  }
  "type checker " must "fail with if expressions and not boolean condition" in {
    var expr = ObSecParser("if 1 then 1 else 2")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeError] {
          TypeChecker(ast)
        }
      }
    }
  }
  "type checker " must "fail with if expressions with branches expression of different types" in {
    var expr = ObSecParser("if true then 1 else false")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeError] {
          TypeChecker(ast)
        }
      }
    }
  }

  "type checker" must "work with multiple method parameters" in {
    var expr = ObSecParser("{z : {ot X {login : String<String String<String -> Int<Int}}<{ot X} => {login password guess = if password.==(guess) then 1 else 0}}.login(\"qwe123\",\"qwe123\")")

    var expectedType = ObSecParser.parseSType("Int<{ot X}")

    var subtyping = new AmadioCardelliSubtyping
    (expr,expectedType) match {
      case (Right(ast),Right(t)) =>
        var aType = TypeChecker(ast)
        assert(subtyping.alphaEq(aType,t))
      case _ => fail("parsing error")
    }
  }
}
