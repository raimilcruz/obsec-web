package scala.ObSecG

import Common.TypeError
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGParser, ObSecGIdentifierResolver}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class TypeCheckerGSpec extends FlatSpec with Matchers with ElementServiceBaseSpec{

  "Type checker with : {z : [ot a {m[T super Int] : Int<T -> Int<T}] => def m(x)= x}" should "work" in{
    val methodType =
      MTypeG(
        List(BoundedLabelVar("T",IntType,ObjectType.top)),
        List(
          ST(IntType,GV("T"))),
        ST(IntType,GV("T")))
    val objType = OT("a",List(MD("m",methodType)))
    val st = ST(objType,objType)
    val expr = Obj("z",st,List(MethodDef("m",List("x"),Var("x"))))

    assert(TypeCheckerG(expr) == st)
  }

  /* "Type checker with : " +
     "{z : [Obj(a)[m<T <: {eq : Int-> Bool}> : T -> Bool]] " +
     "=> " +
     "def m(p)= p.eq(1)}" should "work" in{
     val methodType =
       MTypeG(
         //T<: {eq:Int->Bool}
         List(TypeVarSub("T",OT("r",List(
           MethodDeclarationG(
             "eq",
             MTypeG(List(),List(ST(IntType,IntType)),ST(BooleanType,BooleanType)))
         )))),
         //:T->Bool
         List(ST(GV("T"),GV("T"))),ST(BooleanType,BooleanType))
     val objType = OT("a",List(MD("m",methodType)))
     val st = ST(objType,objType)
     val expr =
       Obj("z",st,
         //def m(p)=> p.eq(1)
         List(MethodDef("m",List("p"),
           MethodInv(
             Var("p"),
             List(),
             List(IntExpr(1)),
             "eq"
           ))))

     assert(TypeCheckerG(expr) == st)
   }
   "Type checker with : " +
     "{z : [Obj(a)[m<T <: Int> : T<Int -> T<Int]] " +
     "=> " +
     "def m(p)= p.+(1)}" should "work" in{
     val methodType =
       MTypeG(
         //T<: {eq:Int->Bool}
         List(
           TypeVarSub("T",IntType)),
         //:T->Bool
         List(ST(GV("T"),IntType)),ST(GV("T"),IntType))
     val objType = OT("a",List(MD("m",methodType)))
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

     assert(TypeCheckerG(expr) == st)
   }
   */

   "Type parameter " should "be in scope for method definition" in{
      val program = "{z : " +
        "{ot a {m[T super Int] : Int<T -> [{m2[T1 super T] : Int<T -> Int<T}]<L }} " +
       "=> " +
       "def m p  = {z1 : " +
        "[{m2[T1 super T] : Int<T -> Int<T}] " +
        "=> def m2 p2 = p2.+[Int](1)}}"

     val otInner = OT("gen",
       List(
         MD(
           "m2",
           MT(
             List(
               BL("T1",LabelVar("T"), ObjectType.top)),
               List(ST(IntType,LabelVar("T"))),
               ST(IntType,LabelVar("T"))
             ))))
     val otOuter =
       OT(
         "a",
         List(
           MD("m",
             MT(
               List(
                 BL("T",IntType,ObjectType.top)),
               List(
                 ST(IntType,LabelVar("T"))),
               ST(otInner,otInner)))))
     val expectedType =  ST(otOuter,otOuter)
     ObSecGParser(program) match{
       case Right(ast)=>
         assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == expectedType)
     }
   }

  "Invocation of add over int " must "work" in {
    var program = "1.+[Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=> assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == ST(IntType,IntType))
    }
  }
  "Invocation of minus over int " must "work" in {
    var program = "1.-[Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=> assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == ST(IntType,IntType))
    }
  }
  "Invocation of eq over int " must "work" in {
    var program = "1.==[Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=> assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == ST(BooleanType,BooleanType))
    }
  }

  "Method invocation with type parameters" should "work" in {
    var program = "{z : {ot X {m[T super Int, T1 : T .. Top ] : Int<T -> Int<T1}}<L => \n def m p  = p.+[Int](1) \n }.m[Int,Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=> assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == ST(IntType,IntType))
    }
  }
  "Method invocation with wrong actual type parameters" should "work" in {
    var program = "{z : {ot X {m[T super Int] : Int<T -> Int<T}}<L => \n def m p  = p \n }.m[String](\"a\")"
    ObSecGParser(program) match{
      case Right(ast)=>
        val expr = ObSecGIdentifierResolver(ast)
        val exp = intercept[TypeErrorG] {
          TypeCheckerG(expr)
        }
        assert(exp.analysisError.errorCode == TypeCheckerErrorCodes.badActualLabelArgument)
    }
  }

  "Invocation of generic method with wrong actual argument" should "work" in {
    var program = "{z : {ot X {m[T super Int] : Int<T -> Int<T}}<L => \n def m p  = p \n }.m[Int](\"a\")"
    ObSecGParser(program) match{
      case Right(ast)=>
        val expr = ObSecGIdentifierResolver(ast)
        val exp = intercept[TypeErrorG] {
          TypeCheckerG(expr)
        }
        assert(exp.analysisError.errorCode == TypeCheckerErrorCodes.subTypingError)
    }
  }
}
