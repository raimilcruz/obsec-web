package ObSec.Static

import ObSec.Ast._
import ObSec.Runtime.{EmptyEnvironment, Environment}

/**
  * Created by racruz on 31-03-2017.
  */
object WellFormedChecker {
  def isWellFormed(stype: SType) = {
    println("isWellFormed. not implemented")
    true
  }

  def isClosed(s: SType): Boolean = isClosed(Environment.empty[Boolean](), s)

  private def isClosed(environment: EmptyEnvironment[Boolean], s: SType): Boolean = {
    isClosed(environment, s.privateType) && isClosed(environment, s.publicType)
  }

  private def isClosed(env: EmptyEnvironment[Boolean], t: Type): Boolean = t match {
    case pt: PrimType => true
    case TypeVar(x) => {
      executeSuccesfully(()=> env.lookup(x))
    }
    case ObjType(x,methods)=> {
      !executeSuccesfully(()=> env.lookup(x.name))

    }
  }

  private def executeSuccesfully[T]( m: () => T): Boolean = {
    try {
      m()
      true
    }
    catch {
      case _: Throwable => false
    }
  }
}
