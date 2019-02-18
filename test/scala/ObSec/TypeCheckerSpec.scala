package scala.ObSec



import Common.{CommonError, NestedScope, Scope}
import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Static._
import ObSecG.Ast.TypeErrorG
import org.scalatest.FlatSpec

/**
  * Created by racruz on 31-03-2017.
  */
class TypeCheckerSpec extends FlatSpec {
  "type-checker" should "work with empty object" in {
    var expr = ObSecParser("{s : {ot X}<{ot X} => }")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(ObjType(TypeVar("X"), List()), ObjType(TypeVar("X"), List()))
        assert(res == expected)
      }
    }
  }

  "type-checker" should "work with objects with methods" in {
    var expr = ObSecParser("{s : {ot X {add : Int<Int -> Int<Int}}<{ot X} => {add x = x.+(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(
          ObjType(
            TypeVar("X"),
            List(MethodDeclaration(
              "add",
              MType(List(SType(IntType, IntType)), SType(IntType, IntType))))),
          ObjType(TypeVar("X"), List()))

        assert(res == expected)
      }
    }
  }
  it should "throw TypeError if an object has repeated methods" in {
    var expr = ObSecParser("{s : {ot X {add : Int<Int -> Int<Int}}<{ot X} => {add x = x.+(x)}{add x = x.-(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[TypeErrorG] {
          TypeChecker(ast)
        }
      }
    }
  }
  it should "throw TypeError if a method is in object type, but is not in the definition" in {
    var expr = ObSecParser("{s : {ot X {minus : Int<Int -> Int<Int}}<{ot X} => {add x = x.-(x)}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[CommonError] {
          TypeChecker(ast)
        }
      }
    }
  }
  it should "throw TypeError if a method is defined, but is not in object type" in {
    var expr = ObSecParser("{s : {ot X }<{ot X} => {add x = x}}")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[CommonError] {
          TypeChecker(ast)
        }
      }
    }
  }
  "type checker " should "work with if expressions" in {
    var expr = ObSecParser("if true then 1 else 2")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        var res = TypeChecker(ast)
        var expected = SType(IntType,IntType)
        assert(res == expected)
      }
    }
  }
  "type checker " must "fail with if expressions and not boolean condition" in {
    var expr = ObSecParser("if 1 then 1 else 2")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[CommonError] {
          TypeChecker(ast)
        }
      }
    }
  }
  "type checker " must "fail with if expressions with branches expression of different types" in {
    var expr = ObSecParser("if true then 1 else false")
    expr match {
      case Left(x) => fail("parsing error")
      case Right(ast) => {
        intercept[CommonError] {
          TypeChecker(ast)
        }
      }
    }
  }

  "type checker" must "work with multiple method parameters" in {
    var expr = ObSecParser("{z : {ot X {login : String<String String<String -> Int<Int}}<{ot X} => {login password guess = if password.==(guess) then 1 else 0}}.login(\"qwe123\",\"qwe123\")")

    var expectedType = ObSecParser.parseSType("Int<{ot X}")

    (expr,expectedType) match {
      case (Right(ast),Right(t)) =>
        var aType = TypeChecker(ast)
        assert(TypeEquivalence.alphaEq(aType,t))
      case _ => fail("parsing error")
    }
  }
  "type checker" must "update to high " in {
    var expr = ObSecParser("if password.==(guess) then 1 else 0")

    var typeChecker = new TypeChecker
    val scope = new NestedScope[SType](new Scope[SType])
    scope.add("password",SType(StringType,ObjType.top))
    scope.add("guess",SType(StringType,StringType))

    expr match {
      case Right(ast) =>
        assert(TypeEquivalence.alphaEq(typeChecker.typeCheck(scope,ast),SType(IntType,ObjType.top)))
      case _ => fail("parsing error")
    }
  }
  "type checker" must "fail " in {
    var expr = ObSecParser("{z : {ot X {login : String<H -> Int<Int}}<L => {login password = if password.==(\"a\") then 1 else 0}}")

    expr match {
      case Right(ast) =>
        intercept[CommonError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }

  "type checker" must "verify well-formedness of security type" in{
    var t =  ObSecParser("{z : {ot x}<Int =>}")
    t match {
      case Right(ast)=>
        intercept[CommonError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }


  "recursive type in deep" must "fail 11" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = z}}")
    expr match {
      case Right(ast)=>
        intercept[CommonError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 11" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<L => {add x = z}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 2" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = x}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }
  "recursive type in deep" must "work 3" in {
    var expr = ObSecParser("{z : {ot x {add : x<x -> x<x}}<{ot x} => {add x = x.add(x)}}")
    expr match {
      case Right(ast)=>
        TypeChecker(ast)
      case _ => fail("parsing error")
    }
  }

  "let expr" should "work with non declaration" in {
    var expr = ObSecParser("let {} in 1.+(2)")
    expr match {
      case Right(ast)=>
         assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }
  "let expr" should "work with one declaration" in {
    var expr = ObSecParser("let {s = \"abc\"} in s.==(\"\")")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }
  "let expr" should "work with three declaration" in {
    var expr = ObSecParser("let {s = \"abc\" s2 = \"124\" res = s.==(s2)} in if res then 1 else 2")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }

  "password policy with hash and eq" must "work" in{
    var expr = ObSecParser("{z : \n{ot X {login : \n String<\n {ot x {hash : -> Int<{ot z {== : Int<Int -> Bool<L}}}} Int<L -> Int<L}}<L => {login password guess = if password.hash().==(guess) then 1 else 0}}.login(\"qwe123\",123)")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }

  "method chain " must "work" in{
    var expr = ObSecParser("password.hash().==(1)")
    var typeT = ObSecParser.parseSType("String<{ot x {hash : -> Int<{ot z {== : Int<Int -> Bool<Bool}}}}")

    var typeChecker = new TypeChecker
    (expr,typeT) match {
      case (Right(ast),Right(st))=>
        val scope = new NestedScope[SType](new Scope)
        scope.add("password",st)
        assert(typeChecker.typeCheck(scope,ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }

  "list constructor" must "work" in{
    var expr = ObSecParser("mklist(\"123\")")
    var typeChecker = new TypeChecker
    expr match {
      case Right(ast) =>
        assert(typeChecker.typeCheck(ast) == SType(StringGListType(StringType),StringGListType(StringType)))
      case _ => fail("parsing error")
    }
  }

  "Method isEmpty on list" must "work" in{
    var expr = ObSecParser("{z : {ot x {f : StrList<L -> Bool<L}}<L => {f l = l.isEmpty()}}")
    var typeT = ObSecParser.parseSType("{ot x {f : StrList<L -> Bool<L}}<L")
    var typeChecker = new TypeChecker
    (expr,typeT) match {
      case (Right(ast),Right(st))=>
        val scope = new NestedScope[SType](new Scope)
        assert(typeChecker.typeCheck(scope,ast) == st)
      case _ => fail("parsing error")
    }
  }
  "Method head on list" must "work" in{
    var expr = ObSecParser("{z : {ot x {f : StrList<L -> String<L}}<L => {f l = l.head()}}")
    var typeT = ObSecParser.parseSType("{ot x {f : StrList<L -> String<L}}<L")
    var typeChecker = new TypeChecker
    (expr,typeT) match {
      case (Right(ast),Right(st))=>
        val scope = new NestedScope[SType](new Scope)
        assert(typeChecker.typeCheck(scope,ast) == st)
      case _ => fail("parsing error")
    }
  }
  "Method tail on list" must "work" in{
    var expr = ObSecParser("{z : {ot x {f : StrList<L -> StrList<L}}<L => {f l = l.tail()}}")
    var typeT = ObSecParser.parseSType("{ot x {f : StrList<L -> StrList<L}}<L")
    var typeChecker = new TypeChecker
    (expr,typeT) match {
      case (Right(ast),Right(st))=>
        val scope = new NestedScope[SType](new Scope)
        assert(typeChecker.typeCheck(scope,ast) == st)
      case _ => fail("parsing error")
    }
  }
  "Use case of list" must "work" in {
    var expr = ObSecParser("{z : \n{ot X {contains : StrList<L -> Bool<L}}<L => \n{contains myList  = if myList.isEmpty() then false else if myList.head().==(\"a\") then true else z.contains(myList.tail()) }}.contains(mklist(\"b\",\"c\",\"a\"))")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }
  "Recursive downgrading policy for list" must "work" in {
    var expr = ObSecParser("{z : {ot X {contains : StrList<{ot r {isEmpty : -> Bool<L}{head: -> String<{ot s {== : String<L -> Bool<L}}}{tail: -> StrList<r}} -> Bool<L}}<L => {contains myList  = if myList.isEmpty() then false else if myList.head().==(\"a\") then true else z.contains(myList.tail()) }}.contains(mklist(\"b\",\"c\",\"a\"))")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }
  "Recursive downgrading policy for list" must "work 2" in {
    var expr = ObSecParser(" let{\n    type StringEq = [{== : String -> Bool}]\n    deftype StrEqList{\n      {isEmpty: -> Bool<L}\n      {head: -> String<StringEq }\n      {tail: -> StrList<StrEqList}\n    }\n    val listHelper = new {z : [{contains : StrList<StrEqList -> Bool<L}] <L  =>\n      def contains myList  =\n      if myList.isEmpty()\n      then false\n      else\n      if myList.head().==(\"a\")\n      then true\n      else z.contains(myList.tail())\n    }\n  }\n  in\n  listHelper.contains(mklist(\"b\",\"c\",\"a\"))")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast) == SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }


  "Invalid arguments" must "fail" in {
    var expr = ObSecParser("\"abc\".hash().==(\"abc\")")
    expr match {
      case Right(ast)=>
        intercept[CommonError]{
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }

  "Type alias" must "work 1" in{
    var expr = ObSecParser("let{\ntype t = [{== : String<L -> Bool<L}]\nauth = new {z : [{login : String<t String<L -> Int<L}]<L \n            => \n            def login password guess = if password.==(guess) then 1 else 0}\n} in \nauth.login(\"qwe123\",\"qwe123\")")
    expr match {
      case Right(ast)=>
          assert(TypeChecker(ast)== SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }
  "Type alias with name that has prefix of bultin type must " must "work" in{
    var expr = ObSecParser("let{\ntype StringEq = [{== : String<L -> Bool<L}]\nauth = new {z : [{login : String<StringEq String<L -> Int<L}]<L \n            => \n            def login password guess = if password.==(guess) then 1 else 0}\n} in \nauth.login(\"qwe123\",\"qwe123\")")
    expr match {
      case Right(ast)=>
        assert(TypeChecker(ast)== SType(IntType,IntType))
      case _ => fail("parsing error")
    }
  }
  "Type alias" must "fail 1" in {
    var expr = ObSecParser("let{\ntype t = [{== : Int<L -> Bool<L}]\nauth = new {z : [{login : String<t String<L -> Int<L}]<L \n            => \n            def login password guess = if password.==(guess) then 1 else 0}\n} in \nauth.login(\"qwe123\",\"qwe123\")")
    expr match {
      case Right(ast) =>
        intercept[CommonError] {
          TypeChecker(ast)
        }
      case _ => fail("parsing error")
    }
  }
  "Dependant type alias" must "work" in{
    var expr = ObSecParser( "let{\n    type StringEq = [{== : String<L -> Bool<L}]\n    type StrEqList = [y \n                        {isEmpty: -> Bool<L}\n                        {head: -> String<StringEq }\n                        {tail: -> StrList<y}\n                                \n                                ]\n    val listHelper = new {z : [\n                    {contains : StrList<StrEqList -> Bool<L}\n                ] <L \n                => \n                def contains myList  = \n                    if myList.isEmpty() \n                    then false \n                    else \n                        if myList.head().==(\"a\") \n                        then true \n                        else z.contains(myList.tail())\n                    \n             }\n} \nin\nlistHelper.contains(mklist(\"b\",\"c\",\"a\"))")
    expr match {
      case Right(ast) =>
        assert(TypeChecker(ast)== SType(BooleanType,BooleanType))
      case _ => fail("parsing error")
    }
  }

  "Deftype" should "work" in {
    var t = ObSecParser("let{\n    deftype StringEq{{== : String<L -> Bool<L}}\n    val auth = new {z : [{login : String<StringEq String<L -> Int<L}]<L \n            => \n            def login password guess = if password.==(guess) then 1 else 0}\n} in \nauth.login(\"qwe123\",\"qwe123\")")
    t match{
      case Right(ast) =>
        assert(TypeChecker(ast) == SType(IntType,IntType))
      case Left(error) => fail(s"parse error: ${error.msg}")
    }
  }


  "Deftype" should "fail" in {
    var t = ObSecParser("let{\n    deftype StringEq{{== : String<Int -> Bool<L}}\n    val auth = new {z : [{login : String<StringEq String<L -> Int<L}]<L \n            => \n            def login password guess = if password.==(guess) then 1 else 0}\n} in \nauth.login(\"qwe123\",\"qwe123\")")
    t match{
      case Right(ast) =>
        intercept[CommonError]{
          TypeChecker(ast)
        }

      case Left(error) => fail(s"parse error: ${error.msg}")
    }
  }
}
