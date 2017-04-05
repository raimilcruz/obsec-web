

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Static.{TypeEquivalence, AmadioCardelliSubtyping, TypeChecker}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 29-03-2017.
  */
class ParserSpec extends FlatSpec with Matchers {
  "parser" should "work with x.m(x)" in {
    val res = ObSecParser("x.m(x)")
    assert(res == Right(MethodInv(Var("x"), List(Var("x")), "m")))
  }
  "parser" should "work with {self : {ot x }<{ot x} => }" in {
    val res = ObSecParser("{self : {ot x }<{ot x} => }")
    assert(res == Right(Obj("self", SType(ObjType(TypeVar("x"), List()), ObjType(TypeVar("x"), List())), List())))
  }
  "parser" should "work with x1" in {
    val res = ObSecParser("x1")
    assert(res == Right(Var("x1")))
  }
  "parser" should "work with an object and object types" in {
    val res = ObSecParser("{z : {ot X\n    {eq : {ot X}<{ot X} -> {ot X}<{ot X} }\n    }<{ot X} =>}")
    var expeted =
      Obj("z",
        SType(
          ObjType(TypeVar("X"),
            List(MethodDeclaration(
              "eq", MType(
                List(SType(
                  ObjType(TypeVar("X"), List()),
                  ObjType(TypeVar("X"), List()))),
                SType(
                  ObjType(TypeVar("X"), List()),
                  ObjType(TypeVar("X"), List())))))),
          ObjType(TypeVar("X"), List())),
        List())
    assert(res == Right(expeted))
  }
  "parser" should "work with an object and object types and method definitions" in {
    val res = ObSecParser("{z : {ot X \n\t\t{eq : {ot X}<{ot X} -> {ot X}<{ot X} }\n\t }<{ot X} \n\t => \n\t{eq x = x}}.eq(z)")
    var expeted =
      MethodInv(
        Obj("z",
          SType(
            ObjType(
              TypeVar("X"),
              List(
                MethodDeclaration("eq",
                  MType(
                    List(SType(
                      ObjType(TypeVar("X"), List()),
                      ObjType(TypeVar("X"), List()))),
                    SType(
                      ObjType(TypeVar("X"), List()),
                      ObjType(TypeVar("X"), List())))))),
            ObjType(TypeVar("X"), List())),
          List(
            MethodDef("eq", List("x"), Var("x")))),
        List(Var("z")),
        "eq")
    assert(res == Right(expeted))
  }
  "parser" should "undestand LOW and HIGH labels" in {

    val res1 = ObSecParser.parseSType("{ot X }<H")
    val res2 = ObSecParser.parseSType("{ot X }<L")

    var expeted1 =
          SType(
            ObjType(
              TypeVar("X"),
              List()),
            ObjType(
              TypeVar("X"),
              List()))
    (res1,res2) match{
      case (Right(r1),Right(r2))=> {
        assert(TypeEquivalence.alphaEq(r1, expeted1))
        assert(TypeEquivalence.alphaEq(r2, expeted1))
      }
      case _ => fail("parse error")
    }

  }

  "parser" should "undestand integers, boolean and string" in {
    val res1 = ObSecParser("15")
    val res2 = ObSecParser("-15")
    val res3 = ObSecParser("\"my favorite string\"")
    val res4 = ObSecParser("false")
    val res5 = ObSecParser("true")

    var expeted1 = IntExpr(15)
    var expeted2 = IntExpr(-15)
    var expeted3 = StringExpr("my favorite string")
    var expeted4 = BooleanExpr(false)
    var expeted5 = BooleanExpr(true)

    assert(res1 == Right(expeted1))
    assert(res2 == Right(expeted2))
    assert(res3 == Right(expeted3))
    assert(res4 == Right(expeted4))
    assert(res5 == Right(expeted5))
  }

  "parser" should "work with if true then 1 else 2" in {
    val res1 = ObSecParser("if true then 1 else 2")

    var expeted1 = IfExpr(BooleanExpr(true),IntExpr(1),IntExpr(2))
    assert(res1 == Right(expeted1))

  }
  "parser " must "accept method type with multiple arguments" in {
    val res1 = ObSecParser.parseType("{ot x {add : Int<Int Int<Int -> Int<Int}}")
    var expeted1 = ObjType(TypeVar("x"),
                    List(MethodDeclaration("add",
                      MType(
                        List(SType(IntType,IntType),SType(IntType,IntType)),
                        SType(IntType,IntType)))))
    assert(res1 == Right(expeted1))

  }
  "parser" must "accept method definition with multiple arguments" in {
    val res1 = ObSecParser("{z : {ot X}<{ot X} => {foo x y = x}}")
    var expeted1 = Obj("z",
                      SType(ObjType(TypeVar("X"),List()),ObjType(TypeVar("X"),List())),
                      List(MethodDef("foo",List("x","y"),Var("x"))))
    assert(res1 == Right(expeted1))

  }
  "parser" must "accept method invocation with multiple arguments" in {
    val res1 = ObSecParser("{z : {ot X}<{ot X} => }.add(x,y)")
    var expeted1 = MethodInv(
                    Obj("z",SType(ObjType(TypeVar("X"),List()),ObjType(TypeVar("X"),List())),List()),
                    List(Var("x"),Var("y")),"add")
    assert(res1 == Right(expeted1))

  }

  "parse" should "accept extended identifier in method names in types" in {
    var t = ObSecParser.parseType("{ot x {== : String<String -> Bool<Bool }}")
    t match{
      case Right(tt) =>
        assert(tt == ObjType(TypeVar("x"),List(MethodDeclaration("==",
        MType(List(SType(StringType,StringType)),SType(BooleanType,BooleanType))))))
      case Left(error) => fail(s"parse error: ${error.msg}")
    }
  }
  "parse" should "work with this" in {
    var t = ObSecParser.parseSType("String<{ot x {== : String<String -> Bool<Bool }}")
    t match{
      case Right(tt) =>
        assert(tt == SType(StringType,ObjType(TypeVar("x"),List(MethodDeclaration("==",
        MType(List(SType(StringType,StringType)),SType(BooleanType,BooleanType)))))))
      case Left(error) => fail(s"parse error: ${error.msg}")
    }
  }
}
