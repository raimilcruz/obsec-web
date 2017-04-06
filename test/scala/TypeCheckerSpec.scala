

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Static._
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

    (expr,expectedType) match {
      case (Right(ast),Right(t)) =>
        var aType = TypeChecker(ast)
        assert(TypeEquivalence.alphaEq(aType,t))
      case _ => fail("parsing error")
    }
  }
  "type checker" must "update to high " in {
    var expr = ObSecParser("if password.==(guess) then 1 else 0")

    var typeChecker = new TypeChecker
    val scope = new NestedScope[SType](new Scope[SType])
    scope.add("password",SType(StringType,ObjType.top))
    scope.add("guess",SType(StringType,StringType))

    expr match {
      case Right(ast) =>
        assert(TypeEquivalence.alphaEq(typeChecker.internalTypeCheck(scope,ast),SType(IntType,ObjType.top)))
      case _ => fail("parsing error")
    }
  }
  "type checker" must "fail " in {
    var expr = ObSecParser("{z : {ot X {login : String<H -> Int<Int}}<L => {login password = if password.==(\"a\") then 1 else 0}}")

    expr match {
      case Right(ast) =>
        intercept[TypeError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }

  "type checker" must "verify well-formedness of security type" in{
    var t =  ObSecParser("{z : {ot x}<Int =>}")
    t match {
      case Right(ast)=>
        intercept[TypeError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }


  "recursive type in deep" must "fail 11" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = z}}")
    expr match {
      case Right(ast)=>
        intercept[TypeError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 11" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<L => {add x = z}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 2" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = x}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 3" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = x.add(x)}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }

  "let expr" should "work with non declaration" in {
    var expr = ObSecParser("let {} in 1.+(2)")
    expr match {
      case Right(ast)=>
         assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }
  "let expr" should "work with one declaration" in {
    var expr = ObSecParser("let {s = \"abc\"} in s.==(\"\")")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }
  "let expr" should "work with three declaration" in {
    var expr = ObSecParser("let {s = \"abc\" s2 = \"124\" res = s.==(s2)} in if res then 1 else 2")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }

  "password policy with hash and eq" must "work" in{
    var expr = ObSecParser("{z : \n{ot X {login : \n String<\n {ot x {hash : -> Int<{ot z {== : Int<Int -> Int<Int}}}} String<L -> Int<L}}<{ot x} => {login password guess = if password.hash().==(guess) then 1 else 0}}.login(\"qwe123\",\"qwe123\")")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }
}
