package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGParser, ObSecGTypeIdentifierResolver}
import ObSecG.Static.TypeCheckerG
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class ResolverGSpec extends FlatSpec with Matchers with BaseSpec {
  "Method invocation with type parameters" should "work" in {
    var program = "{z : {ot X {m[T super Int,T1 super Int] : Int<T -> Int<T1}}<L => \n def m p  = p.+(1) \n }.m[Int,Int](1)"

    ObSecGParser(program) match{
      case Right(ast)=>
        val res = ObSecGTypeIdentifierResolver(ast)
        fail("W")
    }
  }
}
