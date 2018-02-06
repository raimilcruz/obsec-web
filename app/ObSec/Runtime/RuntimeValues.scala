package ObSec.Runtime {

  import Common.Environment
  import ObSec.Ast._

  abstract class RuntimeValue {
    type Evaluator = (Environment[RuntimeValue], ObSecExpr) => RuntimeValue

    def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue
  }

  /**
    * An object closure (containing the environment)
    */
  case class ObjClosure(v: Obj, env: Environment[RuntimeValue]) extends RuntimeValue {
    override def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue = {
      val methDef = this.v.methodImpl(m)
      eval(
        methDef.args.zip(args).foldLeft(Environment.extend(env, v.selfName, ObjClosure(v, env)))(
          (acc :Environment[RuntimeValue], param : (String,RuntimeValue))=> Environment.extend(acc,param._1,param._2)),
        methDef.mBody)
    }

    override def toString: String = "Object closure"
  }

  case class RuntimeInt(v: Int) extends RuntimeValue {
    override def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue = {
      m match {
        case "+" => RuntimeInt(v + (args(0).asInstanceOf[RuntimeInt].v))
        case "-" => RuntimeInt(v - (args(0).asInstanceOf[RuntimeInt].v))
        case "==" => RuntimeBoolean(v == (args(0).asInstanceOf[RuntimeInt].v))
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeStr(v: String) extends RuntimeValue {
    override def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue = {
      m match {
        case "length" => RuntimeInt(v.length)
        case "==" => RuntimeBoolean(v == args(0).asInstanceOf[RuntimeStr].v)
        case "hash" => RuntimeInt(v.hashCode)
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeBoolean(v: Boolean) extends RuntimeValue {
    override def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue =
      throw new StuckError()

    override def toString: String = v.toString
  }
  case class RuntimeStrList(l: List[RuntimeStr]) extends  RuntimeValue{
    override def invoke(m: String, args: List[RuntimeValue], eval: Evaluator): RuntimeValue = m match {
      case "isEmpty" =>  RuntimeBoolean(l.isEmpty)
      case "head" => RuntimeStr(l.head.v)
      case "tail" => RuntimeStrList(l.tail)
      case _ => throw new Error("Stuck")
    }
    override def toString: String = l.toString()

  }

}
