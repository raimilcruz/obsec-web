import ObSec.Ast._

package ObSec.Runtime {

  import ObSec.Ast.{ObSecExpr, Obj}
  import ObSec.Parsing.ObSecParser

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
            val r1 = internalEval(env, e1,steps-1)
            var argValues = args.map(x => internalEval(env, x,steps-1))

            r1.invoke(m, argValues, (e,b)=> internalEval(e,b,steps-1))
          }
          case IfExpr(c, thenPart, elsePart) => {
            var cond = internalEval(env, c,steps-1).asInstanceOf[RuntimeBoolean]
            //TODO: implement as a method invocation over RuntimeBool
            if (cond.v) internalEval(env, thenPart,steps-1)
            else internalEval(env, elsePart,steps-1)
          }
          //PRIMITIVE OPERATIONS
          case primOp: PrimOp => throw new Error("Not supported yet")//primEval(env, primOp)
          case p: SurfaceValExpr => primValEval(p)
          case _ => throw new StuckError(s"Stuck in internalEval with: ${expr}")
        }

   /* private def primEval(env: RuntimeEnv, primOp: PrimOp): RuntimeValue = {
      val op = primOp.op
      val parameters = primOp.operands
      op match {
        case "+" | "-" =>
          if (parameters.length != 2) throw new StuckError()
          else {
            val p1 = internalEval(env, getParam(parameters, 0),steps -1).asInstanceOf[RuntimeInt].v
            val p2 = internalEval(env, getParam(parameters, 1),steps -1).asInstanceOf[RuntimeInt].v
            op match {
              case "+" => RuntimeInt(p1 + p2)
              case "-" => RuntimeInt(p1 - p2)
            }
          }
        case "length" => throw new NotImplementedError()
      }
    }
    */

    def primValEval(primVal: SurfaceValExpr): RuntimeValue = primVal match {
      case IntExpr(n) => RuntimeInt(n)
      case StringExpr(n) => RuntimeStr(n)
      case BooleanExpr(n) => RuntimeBoolean(n)
    }

    private def getParam[T <: ObSecExpr](params: Seq[ObSecExpr], i: Int): T = {
      return params(i).asInstanceOf[T]
    }
  }

  class StuckError(m: String = "") extends Error {

  }

  object Interpreter {
    def apply(x: String) = {
      val expr = ObSecParser(x)
      expr match {
        case Left(e) => Left(e)
        case Right(ast) => Right(new Interpreter().eval(ast))
      }
    }

    def run(x: ObSecExpr) = new Interpreter().eval(x)
  }

}
