package Common.judgment

import Common.judgment.examples.lambdacalculus._

object JudgmentPlayground {
  def main(args:Array[String]):Unit={
    typeCheck(Var("X"))
    typeCheck(Num(1))

    //typeCheck(Lambda("x",IntType,Var("x")))
    typeCheck(Lambda("x",IntType,Var("y")))
    //typeCheck(Lambda("x",IntType,Num(1)))

    typeCheck(App(Lambda("x",IntType,Num(1)),Num(2)))
    typeCheck(App(Num(1),Num(2)))
    typeCheck(App(Lambda("x",IntType,Num(1)),Lambda("x",IntType,Num(1))))

  }
  def typeCheck(term:LambdaCalculusExpr): Unit ={
    println("*****************************")
    println(s"Type-checking ${term.toSource}")
    val result = JudgmentExecutor.stepAll(JudgmentRequest(new TypingEnvironment(),TypingGoal(term),"typing"))
    println(result.state)
  }
}

