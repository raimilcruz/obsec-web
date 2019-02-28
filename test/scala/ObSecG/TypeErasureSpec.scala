package scala.ObSecG

import Common.Environment
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class TypeErasureSpec extends FlatSpec with Matchers with ElementServiceBaseSpec{

  "Type erasure with : {z : [ot a {m[T super Int] : Int<T -> Int<T}] => def m(x)= x}" should "work" in{
    val objectValueInputProgram = "new {z : [{m[T super Int] : Int<T -> Int<T}]<L => def m x = x}"
    val objectValueExpectedProgram = "new {z : [{m : Int<H -> Int<Int}]<L => def m x = x}"

    (ObSecGParser(objectValueInputProgram),ObSecGParser(objectValueExpectedProgram) )match{
      case (Right(input),Right(expected))=>
        val inputAst  = ObSecGIdentifierResolver(input)
        val outputAst  = ObSecGIdentifierResolver(expected)
        assert(TypeErasure.erase(Environment.empty[TypeVarBounds](),inputAst) == outputAst)
    }
  }

}
