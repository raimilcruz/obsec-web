

import ObSec.Ast._
import ObSec.Parsing.ObSecParser
import ObSec.Runtime.{Interpreter, RuntimeInt}
import org.scalatest._



/**
  * Created by racruz on 27-03-2017.
  */
class RuntimeSpec extends FlatSpec with Matchers with GivenWhenThen {
  val interpreter = new Interpreter()
  "eval" should "get 3 with (1 + 2)" in {
    val res = interpreter.eval(PrimOp("+",IntExpr(1),IntExpr(2)))
    assert(res.asInstanceOf[RuntimeInt].v == 3)
  }
  "eval" should "get 3 with 1.+(2)" in {
    val res = interpreter.eval(PrimOp("+",IntExpr(1),IntExpr(2)))
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
}
