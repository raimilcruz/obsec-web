
package ObSecG

import Common.Environment
import ObSec.Ast._
import ObSec.Static.TypeEquivalence
import ObSecG.Ast.{STypeG, TypeErrorG}
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static.TypeCheckerG

object Playground{
  def main(args:Array[String]):Unit= {
    val program = "let{\n    type StringEq = [{== [] : String<I -> Bool<I}]\n    deftype AuthServer {\n        {login: String<StringEq String -> Int}\n    }\n    val auth =  new {z : AuthServer<L =>\n        def login password guess = if password.==(guess) then 1 else 0\n        }\n    }\nin\nauth.login(\"qwe123\",\"qwe123\")"
    ObSecGParser(program) match{
      case Right(ast)=>
        val expr = ObSecGIdentifierResolver(ast)
        //try {
          val theType = TypeCheckerG(expr)
          print(theType)
         // assert(theType == STypeG(IntType, IntType))
        //}catch{
        //  case e : TypeErrorG => print(e.analysisError)
       // }
    }
  }
  /*def join(t1: Type,t2:Type,
           map1: Environment[ObjType],
           map2: Environment[ObjType],
           alreadySeen:List[((Type,Type),Type)]):Type= {
    val findResult = alreadySeen.find((x) => TypeEquivalence.alphaEq(x._1._1, t1) && TypeEquivalence.alphaEq(x._1._2, t2))
    if (findResult.isDefined)
      findResult.get._2
    else {
      case (o1@ObjType(vt1, methods1), o2@ObjType(vt2, methods2)) =>
        val joinVar = TypeVar(freshVar())

        //all common methods with the same amount of arguments.
        val elegibleMethods = methods1.map(m => (m.name, m.mtype.domain)).toSet
          .intersect(methods2.map(m => (m.name, m.mtype.domain)).toSet).toList.map(p => p._1)

        //var elegibleMethods = List[String]()
        val joinedMethods = elegibleMethods.foreach(m => {
          val m1 = methods1.find(meth => meth.name == m).get
          val m2 = methods2.find(meth => meth.name == m).get

          val domain = m1.mtype.domain.zip(m2.mtype.domain).map(pair => {
            SType(meet(pair._1.privateType, pair._2.privateType, map1, map2, alreadySeen),
              meet(pair._1.publicType, pair._2.publicType,, map1, map2, alreadySeen))
          })

        })
        ObjType(joinVar, List())
      case _ => ???
    }
  }
  def meet(t1: Type,t2:Type,
           map1: Environment[ObjType],
           map2: Environment[ObjType],
           resultingMap:List[((Type,Type),Type)]):ObjType= {
   ???
  }
  def freshVar():String = "j"*/
}




/*class Playground {
  def usage1():Unit={
    val a: SecType[SecString,StringConcat]
  }
}
trait StringConcat{
  def concat[lm>: SecString](other: SecType[lm,SecString]): SecType[lm with StringConcat,SecString]
}
trait SecString{
  //def trim() : SecString[self]
  def trim[lm >: SecString](): SecType[lm,SecString]
  def concat[lthis,lm >: SecString](other : SecType[lm,SecString]): SecType[lm with lthis,SecString]
}
case class SecStringImpl(s:String) extends SecString{
  //def value:String = s
  override def trim[lm >: SecString](): SecType[lm, SecString]
  = SecTypeImpl(SecStringImpl(s))

  override def concat[lthis, lm >: SecString](other: SecType[lm, SecString])
    : SecType[lm with lthis, SecString]
  = throw Exception
}

trait SecType[S2, S1 <:S2]{
  def safetyType() : S1
  def declassificationType() : S2
}
case class SecTypeImpl[S2, S1<:S2](val safety:S1) extends SecType[S2,S1] {
  override def safetyType(): S1 = safety

  override def declassificationType(): S2 = safety
}*/
//type ¬[A] = A => Nothing


