package ObSecG.Runtime

import Common.{Environment, StuckError}
import ObSecG.Ast._
import ObSecG.Runtime.ObSec.Runtime._
import org.aopalliance.intercept.MethodInvocation

/**
  * Created by racruz on 24-08-2017.
  */
class InterpreterG {
  type RuntimeEnv = Environment[RuntimeGValue]

  def eval(expr: ObSecGExpr): RuntimeGValue =
    internalEval(Environment.empty[RuntimeGValue](), expr, 200)

  def internalEval(env: RuntimeEnv,
                   expr: ObSecGExpr,
                   steps: Int): RuntimeGValue =
    if (steps < 0) throw new Error("Possible infinite execution")
    else
      expr match {
        //VAR
        case Var(x) => env.lookup(x)
        //OBJ
        case obj@Obj(_, _, _) => ObjGClosure(obj, env)
        //e.m(e)
        case MethodInv(e1,tArgs, args, m) => {
          val r1 = internalEval(env, e1, steps - 1)
          var argValues = args.map(x => internalEval(env, x, steps - 1))

          r1.invoke(m, argValues, (e, b) => internalEval(e, b, steps - 1))
        }
        case IfExpr(c, thenPart, elsePart) => {
          var cond = internalEval(env, c, steps - 1).asInstanceOf[RuntimeGBoolean]
          //TODO: implement as a method invocation over RuntimeBool
          if (cond.v) internalEval(env, thenPart, steps - 1)
          else internalEval(env, elsePart, steps - 1)
        }
        case p: PrimitiveLiteral => primValEval(p)
        case LetStarExpr(declarations,body)=>
          val varDeclaractions = declarations.filter(x=> x.isInstanceOf[LocalDeclaration]).map(d=>d.asInstanceOf[LocalDeclaration])
          val newEnv = varDeclaractions.foldLeft(env)((acc, decl) => Environment.extend(acc,decl.variable,internalEval(acc,decl.rExpr,steps-1)))
          internalEval(newEnv,body,steps-1)
        case ListConstructorExpr(elems) => RuntimeGStrList(elems.map(x=>internalEval(env,x,steps-1).asInstanceOf[RuntimeGStr]))
        case _ => throw new StuckError(s"Stuck in internalEval with: $expr")
      }
  def primValEval(primVal: PrimitiveLiteral): RuntimeGValue = primVal match {
    case IntExpr(n) => RuntimeGInt(n)
    case StringExpr(n) => RuntimeGStr(n)
    case BooleanExpr(n) => RuntimeGBoolean(n)
  }

}
object InterpreterG{
  def run(x:ObSecGExpr): RuntimeGValue = new InterpreterG().eval(x)
}
