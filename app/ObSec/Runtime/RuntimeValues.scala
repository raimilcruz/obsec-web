package ObSec.Runtime {

  import ObSec.Ast._

  abstract class RuntimeValue {
    type Evaluator = (Environment[RuntimeValue], ObSecExpr) => RuntimeValue

    def invoke(m: String, x: RuntimeValue, eval: Evaluator): RuntimeValue
  }

  /**
    * An object closure (containing the environment)
    */
  case class ObjClosure(v: Obj, env: Environment[RuntimeValue]) extends RuntimeValue {
    override def invoke(m: String, arg: RuntimeValue, eval: Evaluator): RuntimeValue = {
      val methDef = this.v.methodImpl(m)
      eval(Environment.extend(
        Environment.extend(env, v.selfName, ObjClosure(v, env)), methDef.argName, arg), methDef.mBody)
    }

    override def toString: String = "Object closure"
  }

  case class RuntimeInt(v: Int) extends RuntimeValue {
    override def invoke(m: String, x: RuntimeValue, eval: Evaluator): RuntimeValue = {
      m match {
        case "+" => RuntimeInt(v + (x.asInstanceOf[RuntimeInt].v))
        case "-" => RuntimeInt(v - (x.asInstanceOf[RuntimeInt].v))
        case "==" => RuntimeBoolean(v == (x.asInstanceOf[RuntimeInt].v))
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeStr(v: String) extends RuntimeValue {
    override def invoke(m: String, x: RuntimeValue, eval: Evaluator): RuntimeValue = {
      m match {
        case "length" => RuntimeInt(v.length)
        case "==" => RuntimeBoolean(v == x.asInstanceOf[RuntimeStr].v)
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeBoolean(v: Boolean) extends RuntimeValue {
    override def invoke(m: String, x: RuntimeValue, eval: Evaluator): RuntimeValue =
      throw new StuckError()

    override def toString: String = v.toString
  }

}
