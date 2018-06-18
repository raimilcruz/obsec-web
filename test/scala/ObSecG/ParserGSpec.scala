package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Parsing.ObSecGParser
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class ParserGSpec extends FlatSpec with Matchers with BaseSpec {
  "Method without label parameters" must "be recognized" in {
    val program = "{ot X {m : Int -> Int}}"
    val res = ObSecGParser.parseType(program)
    res match{
      case Left(x)=> fail(s"Parser should recognize it!. Error: $x")
      case Right(y)=> assert(true)
    }
  }
  "parser with " should "work with {self : {ot x }<{ot x} => }" in {
    val res = ObSecGParser("{self : {ot x }<{ot x} => }")
    assert(res == Right(
      ObjectDefinitionNode("self",
        stA(
          OT("x", List()),
          OT("x", List())), List())))
  }
 "parser" should "work with: " +
    "{z : [{m[T : Int .. Top] : Int<T -> Int<T}]<L " +
    "=> " +
    "def m p  = p.+(1)" +
    "}}" in{
    var program = "{z : {ot X {m[T : Int .. Top] : Int<T -> Int<T}}<L => \n def m p  = p.+(1) \n }"
    val res = ObSecGParser(program)


    val methodType =
      MethodTypeNode(
        //T<: {eq:Int->Bool}
        List(
          bL("T",tI("Int"),tI("Top"))),
        //:T->Bool
        List(stA(tI("Int"),tI("T"))),stA(tI("Int"),tI("T")))
    val objType = OT("X",List(MD("m",methodType)))
    val st = stA(objType,LowLabelNode)

    val expr =
      ObjectDefinitionNode("z",st,
        //def m(p)=> p.add(1)
        List(MDef("m",List("p"),
          MI(
            VariableNode("p"),
            List(),
            List(IntLiteral(1)),
            "+"
          ))))

    assert(res == Right(expr))
  }

  "{z : {ot X {m[T extends Int,T1 extends T ] : T<Int -> T<Int}}<L => \n def m p  = p.+(1) \n }.m[Int](2)"

  "parser" should "work with generic method invocation "in{
    var program = "{z : {ot X {m[T super Int] : Int<T -> Int<T}}<L => \n def m p  = p.+(1) \n }.m[Int](2)"
    val res = ObSecGParser(program)


    val methodType =
      MethodTypeNode(
        //T<: {eq:Int->Bool}
        List(
          superL("T",tI("Int"))),
        //:T->Bool
        List(
          stA(tI("Int"),tI("T"))),
        stA(tI("Int"),tI("T")))
    val objType = OT("X",List(MD("m",methodType)))
    val st = stA(objType,LowLabelNode)

    val expr =
      ObjectDefinitionNode("z",st,
        //def m(p)=> p.add(1)
        List(
          MDef("m",
            List("p"),
            MI(
              VariableNode("p"),
              List(),
              List(IntLiteral(1)),
              "+"
            )))
      )

    var topLevelExpr = MI(expr,List(tI("Int")),List(IntLiteral(2)),"m")
    assert(res == Right(topLevelExpr))
  }

  "Parser" should "accept generic methods" in {
    val program =
      "   [{nil[T extends []] : ->  " +
      "       [X" +
                "{isEmpty : -> Bool}" +
                "{head : -> T}" +
                "{tail : -> X}]} " +
          "]"
    assert(ObSecGParser.parseType(program) ==
      Right(NoRecOT(
        List(
          MD("nil",
            MethodTypeNode(
              List(SubLabelVariableDeclaration("T",
                NoRecOT(List()))),
              List(),
              stA(
                OT("X",
                  List(
                    MD(
                      "isEmpty",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("Bool"),
                          tI("Bool")))),
                    MD(
                      "head",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("T"),
                          tI("T")))),
                    MD(
                      "tail",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("X"),
                          tI("X")))))),
                OT("X",
                  List(
                    MD(
                      "isEmpty",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("Bool"),
                          tI("Bool")))),
                    MD(
                      "head",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("T"),
                          tI("T")))),
                    MD(
                      "tail",
                      MethodTypeNode(
                        List(),
                        List(),
                        stA(
                          tI("X"),
                          tI("X")))))))))))))
  }

  "Union label" should "be recognized " in {
    var program = "{z : {ot X {m: -> X<(X,X)}}<L => \n def m  = z.m() \n}"
    val res = ObSecGParser(program)

    val expected =
      Right(
        ObjectDefinitionNode("z",
          stA(OT("X",
            List(
              MD(
                "m",
                MT(
                  List(),
                  List(),
                  stA(tI("X"),UL(tI("X"),tI("X"))))))),
            LL),
          List(
            MDef(
              "m",
              List(),
              MI("z",List(),List(),"m")))))
    assert(res == expected)
  }

  "StringEq policy with union label" should "be recognized " in {
    var program = "{ot rr {==[T super String]: String<T -> Bool<(Bool,T)}}"
    val res = ObSecGParser.parseType(program)

    val expected = Right(ObjectTypeNode("rr",
      List(MD("==",
        MT(
          List(superL("T",TypeIdentifier("String"))),
          List(stA(TypeIdentifier("String"),TypeIdentifier("T"))),
          stA(TypeIdentifier("Bool"),UnionTypeAnnotation(TypeIdentifier("Bool"),TypeIdentifier("T"))))))))
    assert(res == expected)
  }

  "Method invocation over a method with multiples labels" must "work" in {
    var program = "{z : {ot X {m[T super Int, T1 : T .. Top ] : Int<T -> Int<T1}}<L => \n def m p  = p.+[T](1) \n }.m[Int,Int](1)"
    val res = ObSecGParser(program)
    val expected = Right(
      MI(
        ObjectDefinitionNode("z",
          stA(
            OT(
              "X",
              List(
                MD("m",
                  MT(
                    List(
                      superL(
                        "T",
                        tI("Int")),
                      bL("T1",tI("T"),tI("Top"))),
                    List(stA(tI("Int"),tI("T"))),stA(tI("Int"),tI("T1")))))),LL),
          List(
            MDef(
              "m",
              List("p"),
              MI("p",List(tI("T")),List(IntLiteral(1)),"+")))),List(tI("Int"), tI("Int")),List(IntLiteral(1)),"m"))
    assert(res == expected)
  }
}
