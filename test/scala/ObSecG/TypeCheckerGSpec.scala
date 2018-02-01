package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Static.TypeCheckerG
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class TypeCheckerGSpec extends FlatSpec with Matchers{
  implicit def stringToTypeVar(x:String):TypeVar = TypeVar(x)
  private def ST(t1:TypeG,t2:TypeG)=STypeG(t1,t2)
  private def OT(x:String,methods:List[MethodDeclarationG]) =ObjectType(x,methods)
  private def MD(x:String,mt:MTypeG):MethodDeclarationG=MethodDeclarationG(x,mt)

  "Type checker with : {z : [Obj(a){m<T<:Top> : T -> T}] => def m(x)= x}" should "work" in{
    val methodType = MTypeG(List(TypeVarSub("T",ObjectType.top)),List(ST("T","T")),ST("T","T"))
    val objType = OT("a",List(MD("m",methodType)))
    val st = ST(objType,objType)
    val expr = Obj("z",st,List(n("m",List("x"),Var("x"))))

    assert(TypeCheckerG(expr) == st)
  }

  "Type checker with : {z : [Obj(a)[m:Pair<Int,Bool> -> Int]]  => def m(p)= p.getX + 1}" should "work" in{
    val methodType = MTypeG(List(TypeVarSub("T",ObjectType.top)),List(ST("T","T")),ST("T","T"))
    val objType = OT("a",List(MD("m",methodType)))
    val st = ST(objType,objType)
    val expr = Obj("z",st,List(n("m",List("x"),Var("x"))))

    assert(TypeCheckerG(expr) == st)
  }
}
