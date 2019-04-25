package scala.EObSec

import Common.{CommonError, CommonErrorCodes, ResolverError, ResolverErrorCodes}
import ObSecE.Ast.{ESTypeE, ExistentialType,LetStarExpr}
import ObSecE.Parsing.{EOBSecIdentifierResolver, EObSecParser}

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
class EObsecResolverSpec extends FlatSpec with Matchers with BaseSpec {


  "Method names in an object type " must "be unique" in {
    var objectType = "{ot X {m : -> Int}{m : -> Int}}"
    EObSecParser.parseType(objectType) match{
      case Right(ast)=>
        val thrown = intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.duplicatedMethodInObjectType)
    }
  }
  "Method names in an object " must "be unique" in {
    var program = "{z : {ot X}<L \n=>  \ndef m p  = p\ndef m p  = p\n}"
    EObSecParser(program) match{
      case Right(ast)=>
        val thrown = intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.duplicatedMethodInObject)
    }
  }

  "Value variable " must "be defined" in {
    var program = "{z : {ot X {m : Int -> Int}}<L => \n def m p  = p1 \n }.m(1)"
    EObSecParser(program) match{
      case Right(ast)=>
        var thrown = intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.variableIsNotDefined)
    }
  }

  "Value variable " must "be unique in its scope " in {
    var program = "{z : [{m : Int -> Int}]\n=>  \ndef m p p  = p\n}"
    EObSecParser(program) match{
      case Right(ast)=>
        var thrown = intercept[CommonError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == CommonErrorCodes.variableAlreadyDefinedInScope)
    }
  }



  "Undefined type variable" must "be thrown" in {
    var aType = "T"
    EObSecParser.parseType(aType) match{
      case Right(ast)=>
        var thrown = intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.typeIsNotDefined)
    }
  }

  "Existential variable in existential type" must "be unique" in {
    var existentialType= "exists X super Int,X super Int.[]"
    EObSecParser.parseLabelType(existentialType) match{
      case Right(ast)=>
        val thrown = intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(thrown.analysisError.errorCode == ResolverErrorCodes.variableAlreadyDefined)
    }
  }
  "Existential variable " must "be in scope" in {
    var existentialType= "exists X super Int.[{m: -> Int<X}]"
    EObSecParser.parseLabelType(existentialType) match{
      case Right(ast)=>
        val existentialType =  EOBSecIdentifierResolver(ast)
        assert(existentialType.isInstanceOf[ExistentialType])
    }
  }
  "Existential variable" should "not be in private position" in {
    var existentialType= "exists X super Int.[{m: -> X<Int}]"
    EObSecParser.parseLabelType(existentialType) match{
      case Right(ast)=>
        val exp =intercept[ResolverError] {
          EOBSecIdentifierResolver(ast)
        }
        assert(exp.analysisError.errorCode == ResolverErrorCodes.invalidTypeForPrivateFacet)
    }
  }
  "Existential faceted type" should "be resolved" in {
    var existentialFacetedType= "{ot a} with Int as exists X super Int.[{m: -> Int}]"
    EObSecParser.parseSType(existentialFacetedType) match{
      case Right(ast)=>
        val existentialType =  EOBSecIdentifierResolver(ast)
        assert(existentialType.isInstanceOf[ESTypeE])
    }
  }
  "Existential faceted type with type alias" should "be resolved" in {
    var program= "let{ \n  type E = exists X super Int.[{m: -> Int}] \n  type A = [\n   {m: -> {ot a} with Int as E}\n  ]\n}\nin \n 1"
    EObSecParser(program) match{
      case Right(ast)=>
        val expression =  EOBSecIdentifierResolver(ast)
        assert(expression.isInstanceOf[LetStarExpr])
    }
  }
  "Existential faceted types" should "be automatically open" in {
    //the X type variable in login: String<X -> Int} should be in scope
    var program = s"""let{
      type AccountStore = exists X super Int.[
      {getPassword : -> String<X}
      ]
      type AccountStoreImpl = [
      {getPassword : -> String}
      ]

      deftype AuthServer {
        {method: -> Int}
      }

      val binder = new {
        z: [{bind: AccountStoreImpl with String as AccountStore -> AuthServer<L}]<L
        =>
        def bind store =
        new {z : AuthServer<L
        =>
          def method =
            let{
              val innerAuth = new {
                z1 : [	{login: String<X -> Int}]<L
                =>
                def login p = 1
              }
            }
          in 1
        }
      }
    }
    in 1"""
    EObSecParser(program) match{
      case Right(ast)=>
        val expression =  EOBSecIdentifierResolver(ast)
        assert(expression.isInstanceOf[LetStarExpr])
    }
  }
}
