package ObSecE.Runtime

import Common.{Environment, StuckError}
import ObSecE.Ast._

/**
  * Created by racruz on 24-08-2017.
  */
class EObSecInterpreter {
  type RuntimeEnv = Environment[RuntimeEValue]

  def eval(expr: EObSecExpr): RuntimeEValue =
    internalEval(Environment.empty[RuntimeEValue](), expr, 200)

  def internalEval(env: RuntimeEnv,
                   expr: EObSecExpr,
                   steps: Int): RuntimeEValue =
    if (steps < 0) throw new Error("Possible infinite execution")
    else
      expr match {
        //VAR
        case Var(x) => env.lookup(x)
        //OBJ
        case obj@Obj(_, _, _) => ObjGClosure(obj, env)
        //e.m(e)
        case MethodInv(e1, args, m) => {
          val r1 = internalEval(env, e1, steps - 1)
          var argValues = args.elems.map(x => internalEval(env, x, steps - 1))

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
        case ListConstructorExpr(_,elems) => RuntimeGStrList(elems.map(x=>internalEval(env,x,steps-1).asInstanceOf[RuntimeGStr]))
        case _ => throw new StuckError(s"Stuck in internalEval with: $expr")
      }
  def primValEval(primVal: PrimitiveLiteral): RuntimeEValue = primVal match {
    case IntExpr(n) => RuntimeGInt(n)
    case StringExpr(n) => RuntimeGStr(n)
    case BooleanExpr(n) => RuntimeGBoolean(n)
  }

}
object EObSecInterpreter{
  def run(x:EObSecExpr): RuntimeEValue = new EObSecInterpreter().eval(x)
}

