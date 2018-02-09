package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Parsing.ObSecGParser
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class ParserGSpec extends FlatSpec with Matchers with BaseSpec {
  "parser with " should "work with {self : {ot x }<{ot x} => }" in {
    val res = ObSecGParser("{self : {ot x }<{ot x} => }")
    assert(res == Right(Obj("self", STypeG(ObjectType("x", List()), ObjectType("x", List())), List())))
  }
  "parser" should "work with: " +
    "{z : [{m[T extends Int] : T<Int -> T<Int}]<L " +
    "=> " +
    "def m p  = p.+(1)" +
    "}}" in{
    var program = "{z : {ot X {m[T extends Int] : T<Int -> T<Int}}<L => \n def m p  = p.+(1) \n }"
    val res = ObSecGParser(program)


    val methodType =
      MTypeG(
        //T<: {eq:Int->Bool}
        List(
          TypeVarSub("T",IntType)),
        //:T->Bool
        List(ST(GV("T"),IntType)),ST(GV("T"),IntType))
    val objType = OT("X",List(MD("m",methodType)))
    val st = ST(objType,objType)

    val expr =
      Obj("z",st,
        //def m(p)=> p.add(1)
        List(MethodDef("m",List("p"),
          MethodInv(
            Var("p"),
            List(),
            List(IntExpr(1)),
            "+"
          ))))

    assert(res == Right(expr))
  }

  "{z : {ot X {m[T extends Int,T1 extends T ] : T<Int -> T<Int}}<L => \n def m p  = p.+(1) \n }.m[Int](2)"

  "parser" should "work with generic method invocation "in{
    var program = "{z : {ot X {m[T extends Int] : T<Int -> T<Int}}<L => \n def m p  = p.+(1) \n }.m[Int](2)"
    val res = ObSecGParser(program)


    val methodType =
      MTypeG(
        //T<: {eq:Int->Bool}
        List(
          TypeVarSub("T",IntType)),
        //:T->Bool
        List(ST(GV("T"),IntType)),ST(GV("T"),IntType))
    val objType = OT("X",List(MD("m",methodType)))
    val st = ST(objType,objType)

    val expr =
      Obj("z",st,
        //def m(p)=> p.add(1)
        List(MethodDef("m",List("p"),
          MethodInv(
            Var("p"),
            List(),
            List(IntExpr(1)),
            "+"
          ))))

    var topLevelExpr = MethodInv(expr,List(IntType),List(IntExpr(2)),"m")
    assert(res == Right(topLevelExpr))
  }

  "Parser" should "accept generic methods" in {
    val program =
      "   [{nil[T extends []] : -> \n  " +
      "       [X\n        " +
      "         {isEmpty : -> Bool}\n{head : -> T}\n{tail : -> X}\n ]} \n      " +
    "      {cons[T extends []] : T [X {isEmpty: -> Bool}{head: -> T}{tail: -> X}\n ]\n " +
      "       -> [X {isEmpty: -> Bool}\n{head: -> T}\n{tail: -> X}\n]\n}]"
    assert(ObSecGParser.parseType(program) == Right(Var("x")))
  }
}
