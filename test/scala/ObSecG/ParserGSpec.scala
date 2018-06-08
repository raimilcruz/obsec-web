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
    assert(res == Right(
      ObjectDefinitionNode("self",
        AnnotatedFacetedType(
          ObjectTypeNode("x", List()),
          ObjectTypeNode("x", List())), List())))
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
    val st = stA(objType,tI("L"))

    val expr =
      ObjectDefinitionNode("z",st,
        //def m(p)=> p.add(1)
        List(MethodDefinitionNode("m",List("p"),
          MethodInvocationNode(
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
          SuperLabelVariableDeclaration("T",tI("Int"))),
        //:T->Bool
        List(
          stA(tI("Int"),tI("T"))),
        stA(tI("Int"),tI("T")))
    val objType = OT("X",List(MD("m",methodType)))
    val st = stA(objType,tI("L"))

    val expr =
      ObjectDefinitionNode("z",st,
        //def m(p)=> p.add(1)
        List(
          MethodDefinitionNode("m",
            List("p"),
            MethodInvocationNode(
              VariableNode("p"),
              List(),
              List(IntLiteral(1)),
              "+"
            )))
      )

    var topLevelExpr = MethodInvocationNode(expr,List(tI("Int")),List(IntLiteral(2)),"m")
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
      Right(NoRecursiveObjectTypeNode(
        List(
          MethodDeclarationNode("nil",
            MethodTypeNode(
              List(SubLabelVariableDeclaration("T",
                NoRecursiveObjectTypeNode(List()))),
              List(),
              AnnotatedFacetedType(
                ObjectTypeNode("X",
                  List(
                    MethodDeclarationNode(
                      "isEmpty",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("Bool"),
                          TypeIdentifier("Bool")))),
                    MethodDeclarationNode(
                      "head",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("T"),
                          TypeIdentifier("T")))),
                    MethodDeclarationNode(
                      "tail",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("X"),
                          TypeIdentifier("X")))))),
                ObjectTypeNode("X",
                  List(
                    MethodDeclarationNode(
                      "isEmpty",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("Bool"),
                          TypeIdentifier("Bool")))),
                    MethodDeclarationNode(
                      "head",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("T"),
                          TypeIdentifier("T")))),
                    MethodDeclarationNode(
                      "tail",
                      MethodTypeNode(
                        List(),
                        List(),
                        AnnotatedFacetedType(
                          TypeIdentifier("X"),
                          TypeIdentifier("X")))))))))))))
  }
  "parser" should "recognize self label" in {
    var program = "{z : {ot X {hash : -> X<this}}<L => \n def m  = z.m()\n}"
    val res = ObSecGParser(program)
  }
}
