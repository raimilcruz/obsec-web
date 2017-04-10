

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Runtime._
import ObSec.Static.TypeChecker
import org.scalatest._



/**
  * Created by racruz on 27-03-2017.
  */
class RuntimeSpec extends FlatSpec with Matchers with GivenWhenThen {
  val interpreter = new Interpreter()
  /*"eval" should "get 3 with (1 + 2)" in {
    val res = interpreter.eval(PrimOp("+",IntExpr(1),IntExpr(2)))
    assert(res.asInstanceOf[RuntimeInt].v == 3)
  }*/
  "eval" should "get 3 with 1.+(2)" in {
    val res = interpreter.eval(MethodInv(IntExpr(1),List(IntExpr(2)),"+"))
    assert(res.asInstanceOf[RuntimeInt].v == 3)
  }
  "eval" should "get 3 with {self : {ot X}<L => {add x = 1.+(x)}}.add(3)" in {
    val result = ObSecParser("{self : {ot X}<L => {add x = 1.+(x)}}.add(3)")
    result match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 4)
      }
    }
  }
  "eval" should "get 1 with if true then 1 else 0" in {
    val result = ObSecParser("if true then 1 else 0")
    result match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 1)
      }
    }
  }

  "eval" should "get 3 with if false then 0 else 1.+(2)" in {
    val result = ObSecParser("if false then 0 else 1.+(2)")
    result match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 3)
      }
    }
  }
  "ObSec language " must "have static scope" in{
    Given("{z1 : {ot X}<L => {f x = x.g(2)}}." +
      "f({z2 : {ot X}<L => {g x = x.+(1)}})")
    val expr = ObSecParser("{z1 : {ot X}<L => {f x = x.g(2)}}.f({z2 : {ot X}<L => {g x = x.+(1)}})")
    When("it")
    Then("the result is OK")
    expr match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 3)
      }
    }
  }
  "ObSec language " must "have static scope (2)" in{
    Given("{z1 : {ot X}<L => {f x = x.g(2)}}." +
      "f({z2 : {ot X}<L => {g x = x.+(1)}})")
    val expr = ObSecParser("{z1 : {ot X}<L =>\n   {apply x = {z2 : {ot X}<L =>\n\t\t\t\t{apply y = {z3 : {ot X}<L => \n\t\t\t\t\t{apply x = x.+(y)}}.apply(x.+(x))}}.apply(x.+(x))}}.apply(3.+(2))")
    When("it")
    Then("the result is OK")
    expr match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 20)
      }
    }
  }


  "Recursion over self" must "work" in {
    val expr = ObSecParser("{z : {ot X }<L => \n\t{sum x = if x.==(0) then 0 else x.+(z.sum(x.-(1)))}}.sum(5)")
    expr match {
      case Left(error) => fail(s"We expected an ast $error")
      case Right(ast)=> {
        val res = interpreter.eval(ast)
        assert(res.asInstanceOf[RuntimeInt].v == 15)
      }
    }
  }
  "Multiple parameters" must "work 1" in {
    val expr1 = ObSecParser("{z : {ot X {login : String<String String<String -> Bool<Bool}}<L =>" +
      "{login password guess = if password.==(guess) then 1 else 0}}.login(\"qwe123\",\"qwe123\")")
    val expr2 = ObSecParser("{z : {ot X {login : String<String String<String -> Bool<Bool}}<L =>" +
      "{login password guess = if password.==(guess) then 1 else 0}}.login(\"qwe123\",\"abc\")")
    (expr1,expr2) match {
      case (Right(ast1),Right(ast2)) =>
        val res = interpreter.eval(ast1)
        assert(res.asInstanceOf[RuntimeInt].v == 1)

        val res2 = interpreter.eval(ast2)
        assert(res2.asInstanceOf[RuntimeInt].v == 0)
      case (Left(error),_) => fail(s"We expected an ast $error")
    }
  }
  "let expr" should "work with one declaration" in {
    var expr = ObSecParser("let {s = \"abc\"} in s.==(\"\")")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeBoolean(false))
      case _ => fail("parsing error")
    }
  }
  "let expr" should "work with three declaration" in {
    var expr = ObSecParser("let {s = \"abc\" s2 = \"124\" res = s.==(s2)} in if res then 1 else 2")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeInt(2))
      case _ => fail("parsing error")
    }
  }
  "List constructor" should "work" in{
    var expr = ObSecParser("mklist(\"abc\")")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeStrList(List(RuntimeStr("abc"))))
      case _ => fail("parsing error")
    }
  }
  "List isEmpty" should "work" in{
    var expr = ObSecParser("mklist(\"abc\").isEmpty()")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeBoolean(false))
      case _ => fail("parsing error")
    }
  }

  "List head" should "work" in{
    var expr = ObSecParser("mklist(\"abc\").head()")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeStr("abc"))
      case _ => fail("parsing error")
    }
  }
  "List tail" should "work" in{
    var expr = ObSecParser("mklist(\"abc\").tail()")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeStrList(List()))
      case _ => fail("parsing error")
    }
  }
  "Use case of list" should "work" in{
    var expr = ObSecParser("{z : \n{ot X {contains : StrList<L -> Bool<L}}<L => \n{contains myList  = if myList.isEmpty() then false else if myList.head().==(\"a\") then true else z.contains(myList.tail()) }}.contains(mklist(\"b\",\"c\",\"a\"))")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeBoolean(true))
      case _ => fail("parsing error")
    }
  }
  "Example: Program policy with hash and eq  " must "work" in {
    var expr = ObSecParser("let {\nauth = {z : {ot X \n                {login : String<{ot x \n                                    {hash : -> Int<{ot z \n                                                    {== : Int<Int -> Bool<L}}}} String<L -> Int<L}}<L \n        => \n            {login password guess = if password.hash().==(guess) then 1 else 0}}\n    \n} in\nauth.login(\"qwe123\",\"qwe123\")")
    expr match {
      case Right(ast)=>
        assert(interpreter.eval(ast) == RuntimeBoolean(true))
      case _ => fail("parsing error")
    }
  }
}
