package Common.judgment.examples.lambdacalculus

import Common.judgment._

trait SimpleType extends MetaExpr{
}
object IntType extends SimpleType {
  override def mathify: String = "\\Int"

  override def toSource: String = "Int"

  override def children: List[MetaExpr] = List()

  override def replace(l: List[(Int, MetaExpr)]): MetaExpr = IntType
}

case class Arrow(t1:MetaExpr,t2:MetaExpr) extends  SimpleType {
  override def mathify: String = s"${t1.mathify} \\rightarrow ${t2.mathify}"

  override def toSource: String = s"${t1.toSource} -> ${t2.toSource}"

  override def children: List[MetaExpr] = t1.children ++ t2.children

  override def replace(l: List[(Int, MetaExpr)]): MetaExpr = Arrow(t1.replace(l),t2.replace(l))
}

class TypingEnvironment(val mapping: Map[String,SimpleType] = Map()) extends JudgmentContext{
  override def judgmentKey: String = "typing"
  def contains(x:String):Boolean = mapping.contains(x)
  def get(x:String):SimpleType = mapping(x)
  def extend(x:String,t:SimpleType):TypingEnvironment = new TypingEnvironment(mapping + (x -> t))

  def mathify:String= mapping.map(p=> s"${p._1}:${p._2.toSource}").mkString(",")
}

case class TypingGoal(e:LambdaCalculusExpr) extends JudgmentGoal {
  override def judgmentKey: String = "typing"
}
case class TypingJudgmentOutput(t:SimpleType) extends JudgmentOutput {
  override def mathify: String = t.mathify
  override def metaExpr: MetaExpr = t
}
case class TypingPremise(context:TypingEnvironment, goal:TypingGoal) extends JudgmentPremise {
  override def key: String = "typing"

  override def output: JudgmentOutput = NoOutput
}
case class TypeEqPredicate(t1:MetaExpr,t2:MetaExpr) extends JudgmentPredicate {
  override def key: String = "lambda-calculus-type-eq"
  override def metaTerms: List[MetaExpr] = List(t1,t2)

  override def replace(l: List[(Int, MetaExpr)]): JudgmentPredicate =
    TypeEqPredicate(t1.replace(l),t2.replace(l))
}
case class CodomainExpr(t:MetaExpr) extends MetaExpr with MetaFunction {
  override def toSource: String = s"cod(${t.toSource})"

  override def children: List[MetaExpr] = List(t)

  override def mathify: String = s"cod(${t.mathify})"

  override def replace(l: List[(Int, MetaExpr)]): MetaExpr = CodomainExpr(t.replace(l))

  override def key: String = "codomain"
}
case class DomainExpr(t:MetaExpr) extends MetaExpr with MetaFunction {
  override def toSource: String = s"dom(${t.toSource})"

  override def children: List[MetaExpr] = List(t)

  override def mathify: String = s"dom(${t.mathify})"

  override def replace(l: List[(Int, MetaExpr)]): MetaExpr = DomainExpr(t.replace(l))

  override def key: String = "domain"
}


trait LambdaCalculusExpr extends{
  def mathify:String
  def toSource:String
}
case class Lambda(x:String, t:SimpleType,b:LambdaCalculusExpr) extends LambdaCalculusExpr {
  override def mathify: String = s"\\lambda $x: ${t.mathify}. ${b.mathify}"

  override def toSource: String = s"lam $x : ${t.toSource}.  ${b.toSource}"
}
case class Num(n:Int) extends LambdaCalculusExpr {
  override def mathify: String = n.toString

  override def toSource: String = n.toString
}
case class Var(x:String) extends LambdaCalculusExpr {
  override def mathify: String = x

  override def toSource: String = x.toString
}
case class App(e1:LambdaCalculusExpr,e2:LambdaCalculusExpr) extends LambdaCalculusExpr {
  override def mathify: String = s"${e1.mathify} ~ ${e2.mathify}"

  override def toSource: String = s"${e1.toSource} ${e2.toSource}"
}
case class ParExpr(e:LambdaCalculusExpr) extends LambdaCalculusExpr {
  override def mathify: String = s"(${e.mathify})"

  override def toSource: String = s"(${e.toSource})"
}

class TypingJudgmentImplementation{
  def typeOf(env:TypingEnvironment, expr: LambdaCalculusExpr): JudgmentStep = expr match {
    case Num(n) => JudgmentStepBaseCase(TypingGoal(expr),env,TypingJudgmentOutput(IntType))
    case Var(x) =>
      if(env.contains(x))
        JudgmentStepBaseCase(TypingGoal(expr),env,TypingJudgmentOutput(env.get(x)))
      else
        JudgmentStepNoRuleCase(TypingGoal(expr),env,NoOutput)
    case Lambda(x,t,b)=>
      JudgmentStepRecursiveCase(TypingGoal(expr),
        List(TypingPremise(env.extend(x,t),TypingGoal(b))),
        env,List(), PendingOutput(Arrow(t,JudgmentOutputRef(1))))
    case App(e1,e2)=>
      JudgmentStepRecursiveCase(TypingGoal(expr),
        List(TypingPremise(env,TypingGoal(e1)), TypingPremise(env,TypingGoal(e2))),env,
        List(TypeEqPredicate(DomainExpr(JudgmentOutputRef(1)),JudgmentOutputRef(2))),
        PendingOutput(CodomainExpr(JudgmentOutputRef(1))))
    case ParExpr(e)=> typeOf(env,e)

  }
  def functionAppArgumentPredicate(t1:SimpleType,t2:SimpleType):Boolean = t1 match{
    case Arrow(e1,e2) => e1 == e2
    case _  => false
  }
}

class TypingJudgmentAdapter extends JudgmentAdapter{
  override def key: String = "typing"

  override def step(request: JudgmentRequest): JudgmentStep =  request match {
    case JudgmentRequest(ctx, goal,jkey) =>
      if(!goal.isInstanceOf[TypingGoal] || !ctx.isInstanceOf[TypingEnvironment])
        JudgmentStepNoRuleCase(goal,ctx)
      else {
        val judgmentGoal = goal.asInstanceOf[TypingGoal]
        val judgmentContext = ctx.asInstanceOf[TypingEnvironment]
        val judgmentImplementation = new TypingJudgmentImplementation()
        judgmentImplementation.typeOf(judgmentContext,judgmentGoal.e)
      }
  }
}
class TypeEqPredicateAdapter extends PredicateAdapter{
  override def key: String = "lambda-calculus-type-eq"

  override def eval(args: List[MetaExpr]):  Either[Boolean,String] = args match {
    case List(t1:SimpleType,t2:SimpleType) => Left(t1 == t2)
    case _ => throw new UnsupportedOperationException(s"$key predicate requires two instances of SimpleType")
  }
}
class DomainFunctionAdapter extends FunctionAdapter{
  override def key: String = "domain"
  override def eval(args: List[MetaExpr]): Either[MetaExpr, String] = args match{
    case List(t:SimpleType)=>
      t match {
        case Arrow(t1,t2) => Left(t1)
        case otherType => Right(s"domain is not defined for $otherType")
      }
    case _ => Right(s"domain predicate requires an instance of SimpleType")
  }
}
class CodomainFunctionAdapter extends FunctionAdapter{
  override def key: String = "codomain"
  override def eval(args: List[MetaExpr]): Either[MetaExpr, String] = args match{
    case List(t:SimpleType)=>
      t match {
        case Arrow(t1,t2) => Left(t2)
        case otherType => Right(s"codomain is not defined for $otherType")
      }
    case _ => Right(s"codomain predicate requires an instance of SimpleType")
  }
}

