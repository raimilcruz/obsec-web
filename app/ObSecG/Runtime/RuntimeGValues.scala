package ObSecG.Runtime

package ObSec.Runtime {

  import Common.{Environment, PrettyPrint, StuckError}
  import ObSecG.Ast._

  abstract class RuntimeGValue extends PrettyPrint {
    type Evaluator = (Environment[RuntimeGValue], ObSecGExpr) => RuntimeGValue

    def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue

    override def prettyPrint(builder: StringBuilder): Unit =
      builder.append(toString)
  }

  /**
    * An object closure (containing the environment)
    */
  case class ObjGClosure(v: Obj, env: Environment[RuntimeGValue]) extends RuntimeGValue {
    override def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue = {
      val methDef = this.v.methodImpl(m)
      eval(
        methDef.args.zip(args).foldLeft(Environment.extend(env, v.selfName, ObjGClosure(v, env)))(
          (acc :Environment[RuntimeGValue], param : (String,RuntimeGValue))=> Environment.extend(acc,param._1,param._2)),
        methDef.mBody)
    }

    override def toString: String = "Object closure"
  }

  case class RuntimeGInt(v: Int) extends RuntimeGValue {
    override def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue = {
      m match {
        case "+" => RuntimeGInt(v + (args(0).asInstanceOf[RuntimeGInt].v))
        case "-" => RuntimeGInt(v - (args(0).asInstanceOf[RuntimeGInt].v))
        case "==" => RuntimeGBoolean(v == (args(0).asInstanceOf[RuntimeGInt].v))
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeGStr(v: String) extends RuntimeGValue {
    override def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue = {
      m match {
        case "length" => RuntimeGInt(v.length)
        case "==" => RuntimeGBoolean(v == args.head.asInstanceOf[RuntimeGStr].v)
        case "hash" => RuntimeGInt(v.hashCode)
        case "first" => RuntimeGStr(v.head.toString)
        case "concat" => RuntimeGStr(v.concat(args.head.asInstanceOf[RuntimeGStr].v))
        case _ => throw new StuckError()
      }
    }

    override def toString: String = v.toString
  }

  case class RuntimeGBoolean(v: Boolean) extends RuntimeGValue {
    override def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue =
      throw new StuckError()

    override def toString: String = v.toString
  }
  case class RuntimeGStrList(l: List[RuntimeGStr]) extends  RuntimeGValue{
    override def invoke(m: String, args: List[RuntimeGValue], eval: Evaluator): RuntimeGValue = m match {
      case "isEmpty" =>  RuntimeGBoolean(l.isEmpty)
      case "head" => RuntimeGStr(l.head.v)
      case "tail" => RuntimeGStrList(l.tail)
      case _ => throw new Error("Stuck")
    }
    override def toString: String = l.toString()

  }

}
