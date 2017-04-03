

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 29-03-2017.
  */
class ParserSpec extends FlatSpec with Matchers {
  "parser" should "work with x.m(x)" in {
    val res = ObSecParser("x.m(x)")
    assert(res == Right(MethodInv(Var("x"), Var("x"), "m")))
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
                SType(
                  ObjType(TypeVar("X"), List()),
                  ObjType(TypeVar("X"), List())),
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
                    SType(
                      ObjType(TypeVar("X"), List()),
                      ObjType(TypeVar("X"), List())),
                    SType(
                      ObjType(TypeVar("X"), List()),
                      ObjType(TypeVar("X"), List())))))),
            ObjType(TypeVar("X"), List())),
          List(
            MethodDef("eq", "x", Var("x")))),
        Var("z"),
        "eq")
    assert(res == Right(expeted))
  }
  "parser" should "undestand LOW and HIGH labels" in {
    val res1 = ObSecParser("{z : {ot X }<H =>}")
    val res2 = ObSecParser("{z : {ot X }<L =>}")
    var expeted1 =
        Obj("z",
          SType(
            ObjType(
              TypeVar("X"),
              List()),
            HighLabel),
          List())
    var expeted2 =
      Obj("z",
        SType(
          ObjType(
            TypeVar("X"),
            List()),
          LowLabel),
        List())
    assert(res1 == Right(expeted1))
    assert(res2 == Right(expeted2))
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
  /*"primitive types" should "work" in {
    val res1 = ObSecParser("{z1 : Int<Int => }")
    var expeted1 = Obj("z1",SType(IntType,IntType)BooleanExpr(true),IntExpr(1),IntExpr(2))
    assert(res1 == Right(expeted1))

  }*/
}
