package ObSec.Static

import ObSec.Ast._
import ObSec.Runtime.{EmptyEnvironment, Environment}

/**
  * Created by racruz on 31-03-2017.
  */
class WellFormedChecker(val errorCollector : ErrorCollector) {
  def isWellFormed(stype: SType) = {
    val sb = new AmadioCardelliSubtyping
    isClosed(stype) && (sb.<::(stype.privateType,stype.publicType) ||
      {
        errorCollector.report(s"Private facet must be subtype of public facet in security type: ${stype}")
        false
      })
  }

  def isClosed(s: SType): Boolean = isSClosed(Environment.empty[Boolean](), s)

  private def isSClosed(environment: Environment[Boolean], s: SType): Boolean = {
    isClosed(environment, s.privateType) && isClosed(environment, s.publicType)
  }

  private def isClosed(env: Environment[Boolean], t: Type): Boolean = t match {
    case pt: PrimType => true
    case TypeVar(x) =>
      executeSuccesfully(()=> env.lookup(x)) || {
        errorCollector.report(s"Free type variable ${x}")
        false
      }
    case ObjType(x,methods)=> {
      if(executeSuccesfully(() => env.lookup(x.name)))false
      val newEnv = Environment.extend(env,x.name,true)
      methods.forall(m => isSClosed(newEnv,m.mtype.codomain) && m.mtype.domain.forall(x=> isSClosed(newEnv,x)))
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
