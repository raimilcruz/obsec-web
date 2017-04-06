import ObSec.Ast._

package ObSec.Runtime {

  import ObSec.Ast.{ObSecExpr, Obj}
  import ObSec.Parsing.{ObSecParser, ObSecParserError}

  /**
    * Defines an interpreter for the ObSe language
    */
  class Interpreter {
    type RuntimeEnv = Environment[RuntimeValue]

    /**
      * Evaluates an expression (in big step fashion)
      *
      * @param expr The expression to eval
      * @return
      */
    def eval(expr: ObSecExpr) = internalEval(Environment.empty[RuntimeValue](), expr, 200)


    def internalEval(env: RuntimeEnv, expr: ObSecExpr, steps: Int): RuntimeValue =
      if (steps < 0) throw new Error("Possible infinite execution")
      else
        expr match {
          //VAR
          case Var(x) => env.lookup(x)
          //OBJ
          case obj@Obj(_, _, _) => ObjClosure(obj, env)
          //e.m(e)
          case MethodInv(e1, args, m) => {
            val r1 = internalEval(env, e1, steps - 1)
            var argValues = args.map(x => internalEval(env, x, steps - 1))

            r1.invoke(m, argValues, (e, b) => internalEval(e, b, steps - 1))
          }
          case IfExpr(c, thenPart, elsePart) => {
            var cond = internalEval(env, c, steps - 1).asInstanceOf[RuntimeBoolean]
            //TODO: implement as a method invocation over RuntimeBool
            if (cond.v) internalEval(env, thenPart, steps - 1)
            else internalEval(env, elsePart, steps - 1)
          }
          case p: SurfaceValExpr => primValEval(p)
          case LetStarExpr(declarations,body)=>
            val newEnv = declarations.foldLeft(env)((acc, decl) => Environment.extend(acc,decl.variable,internalEval(acc,decl.rExpr,steps-1)))
            internalEval(newEnv,body,steps-1)
          case _ => throw new StuckError(s"Stuck in internalEval with: ${expr}")
        }

    def primValEval(primVal: SurfaceValExpr): RuntimeValue = primVal match {
      case IntExpr(n) => RuntimeInt(n)
      case StringExpr(n) => RuntimeStr(n)
      case BooleanExpr(n) => RuntimeBoolean(n)
    }
  }

  class StuckError(m: String = "") extends Error {

  }

  object Interpreter {
    def apply(x: String): Either[ObSecParserError, RuntimeValue] = {
      val expr = ObSecParser(x)
      expr match {
        case Left(e) => Left(e)
        case Right(ast) => Right(new Interpreter().eval(ast))
      }
    }

    def run(x: ObSecExpr): RuntimeValue = new Interpreter().eval(x)
  }

}
