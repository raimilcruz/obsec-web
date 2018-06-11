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

  "Type checker with : {z : [Obj(a){m[T super Int] : Int<T -> Int<T}] => def m(x)= x}" should "work" in{
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

   "Type parameter " should "be in scope for method definition" in{
      /*"{z : [Obj(a)[m<T <: Int> : T<Int -> [{m2<T1<: T> : T<Int -> T<T}]<L ]] " +
       "=> " +
       "def m(p)= {z1 : [{m2<T1<: T> : T<Int -> T<T}]
                         =>
                         def m2(p2} = p2.add(1)}"
       */
      val innerOt =
        OT("b",
          List(
            MD("m2",
              MTypeG(
                List(TypeVarSub("T1",GV("T"))),
                List(ST(GV("T"),IntType)),
                ST(GV("T"),GV("T"))
              ))
          ))
      var innerSt =
        ST(
          innerOt,innerOt)
       val methodType =
         MTypeG(
           //T<: {eq:Int->Bool}
           List(
             TypeVarSub("T",IntType)),
           //:T->Bool
           List(ST(GV("T"),IntType)),
           innerSt
          )
       val objType = OT("a",List(MD("m",methodType)))
       val st = ST(objType,objType)
       val innerObj =
       Obj("z1",innerSt,
         //def m(p)=> p.add(1)
         List(MethodDef("m2",List("p2"),
           MethodInv(
             Var("p2"),
             List(),
             List(IntExpr(1)),
             "+"
           ))))

       val expr =
         Obj("z",st,
         //def m(p)=> {z1 : ...}
         List(MethodDef("m",List("p"),innerObj)))
       assert(TypeCheckerG(expr) == st)
   }*/
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
    var program = "{z : {ot X {m[T super Int, T1 : T .. Top ] : Int<T -> Int<T1}}<L => \n def m p  = p.+(1) \n }.m[Int,Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=> assert(TypeCheckerG(ObSecGIdentifierResolver(ast)) == ST(IntType,IntType))
    }
  }
  "Type substitution for generic variable " should "work" in {
    var program = "{z : {ot X {m[T extends []] : T -> T}}<L => \n def m p  = p \n }.m[String](1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        intercept[TypeError] {
          var expr = ObSecGIdentifierResolver(ast)
          TypeCheckerG(expr)
        }
    }
  }
}
