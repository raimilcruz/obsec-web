package scala.ObSecG

import Common.TypeError
import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static.TypeCheckerG
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class ResolverGSpec extends FlatSpec with Matchers with BaseSpec {

  "Label variable" should "not be in private position" in {
    var program = "{z : {ot X {m[T extends []] : T -> T}}<L => \n def m p  = p \n }.m[Int](1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        val exp =intercept[ResolverError] {
          ObSecGIdentifierResolver(ast)
        }
        assert(exp.analysisError.errorCode == ResolverErrorCodes.invalidTypeForPrivateFacet)
    }
  }
  "Method names in an object type " must "be unique" in {
    var program = "{z : {ot X {m : Int -> Int}{m : Int -> Int}}<L => \n def m p  = p \n }.m(1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        val thrown = intercept[ResolverError] {
          ObSecGIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.duplicatedMethodInObjectType)
    }
  }

  "Label variable names in a method declaraton" must "be unique" in {
    var program = "{z : {ot X {m[T super Int, T super Int] : Int -> Int}}<L => \n def m p  = p \n }.m(1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        val thrown = intercept[ResolverError] {
          ObSecGIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.variableAlreadyDefined)
    }
  }
  "Value variable " must "be defined" in {
    var program = "{z : {ot X {m[T super Int] : Int -> Int}}<L => \n def m p  = p1 \n }.m(1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        var thrown = intercept[ResolverError] {
          ObSecGIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.variableIsNotDefined)
    }
  }
  "Type variable " must "be defined" in {
    var program = "{z : {ot X {m : Int<T -> Int}}<L => \n def m p  = p \n }.m(1)"
    ObSecGParser(program) match{
      case Right(ast)=>
        var thrown = intercept[ResolverError] {
          ObSecGIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.typeIsNotDefined)
    }
  }


  "Method invocation with type parameters" should "work" in {
    var program = "{z : {ot X {m[T super Int,T1 super T] : Int<T -> Int<T1}}<L => \n def m p  = p.+(1) \n }.m[Int,Int](1)"

    ObSecGParser(program) match{
      case Right(ast)=>
        val res = ObSecGIdentifierResolver(ast)
        assert(true)
    }
  }
}
