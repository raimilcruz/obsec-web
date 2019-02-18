package Common.judgment.examples

import Common.judgment._

//usage
sealed trait Nat{
  def toInt : Int = Nat.natToNumber(this)
}
object Nat{
  def numberIsNat(n:Int):Boolean = n>=0

  def numberToNat(n:Int):Nat=
    if(n<=0) Zero
    else Succ(numberToNat(n-1))
  def natToNumber(n:Nat):Int = n match{
    case Zero => 0
    case Succ(n1) => natToNumber(n1)+1
  }

}
object Zero extends Nat{
  override def toString: String = "0"
}
case class Succ(n:Nat) extends Nat{
  override def toString: String = s"S ${n.toString}"
}



case class LessEqJudgmentGoal(n1:Nat,n2:Nat) extends JudgmentGoal {
  override def judgmentKey: String = "less-eq"
}
case class LessEqPremise(context: JudgmentContext,goal:LessEqJudgmentGoal) extends JudgmentPremise {
  override def key: String = "less-eq"

  override def output: JudgmentOutput = NoOutput
}

case class EqJudgmentGoal(n1:Nat,n2:Nat) extends JudgmentGoal {
  override def judgmentKey: String = "eq"
}

case class EqPremise(context: JudgmentContext,goal:EqJudgmentGoal) extends JudgmentPremise {
  override def key: String = "eq"

  override def output: JudgmentOutput = NoOutput
}

class JudgmentExample{
  //two cases:
  //
  // --------------- [TZero]
  //   Zero <= N
  //
  //    N1 <= N2
  // --------------- [TSucc]
  //   Succ N1 <= Succ N2
  //
  def lEq(n1:Nat, n2:Nat): JudgmentStep = n1 match {
    case Zero => JudgmentStepBaseCase(LessEqJudgmentGoal(n1,n2))
    case Succ(n11)=> n2 match{
      case Succ(n12) => JudgmentStepRecursiveCase(LessEqJudgmentGoal(n1,n2),
        List(
          LessEqPremise(EmptyJudgmentContext, LessEqJudgmentGoal(n11,n12)),
          EqPremise(EmptyJudgmentContext, EqJudgmentGoal(n11,n12))
      ))
      case _ => JudgmentStepNoRuleCase(LessEqJudgmentGoal(n1,n2))
    }
  }

  def eq(n1:Nat, n2:Nat): JudgmentStep = (n1,n2) match {
    case (Zero,Zero) => JudgmentStepBaseCase(EqJudgmentGoal(n1,n2))
    case (Succ(n11),Succ(n12))=>
      JudgmentStepRecursiveCase(EqJudgmentGoal(n1,n2),
        List(EqPremise(EmptyJudgmentContext, EqJudgmentGoal(n11,n12))))
    case _ => JudgmentStepNoRuleCase(EqJudgmentGoal(n1,n2))
  }
}

class LessEqJudgmentAdapter extends JudgmentAdapter{
  override def key: String = "less-eq"

  override def step(request: JudgmentRequest): JudgmentStep =  request match {
    case JudgmentRequest(ctx, goal,jkey) =>
      if(!goal.isInstanceOf[LessEqJudgmentGoal])
        JudgmentStepNoRuleCase(goal)
      else {
        val lessEqGoal = goal.asInstanceOf[LessEqJudgmentGoal]
        val exampleJudgment = new JudgmentExample()
        exampleJudgment.lEq(lessEqGoal.n1,lessEqGoal.n2)
      }
  }

}
class EqJudgmentAdapter extends JudgmentAdapter{
  override def key: String = "eq"

  override def step(request: JudgmentRequest): JudgmentStep =  request match {
    case JudgmentRequest(ctx, goal,jkey) =>
      if(!goal.isInstanceOf[EqJudgmentGoal])
        JudgmentStepNoRuleCase(goal)
      else {
        val eqGoal = goal.asInstanceOf[EqJudgmentGoal]
        val exampleJudgment = new JudgmentExample()
        exampleJudgment.eq(eqGoal.n1, eqGoal.n2)
      }
  }
}