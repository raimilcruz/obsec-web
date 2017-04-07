package ObSec.Static

import ObSec.Ast._
import ObSec.Runtime.{EmptyEnvironment, Environment}

/**
  * Created by racruz on 31-03-2017.
  */
class WellFormedChecker(val errorCollector : ErrorCollector) {
  val sb = new AmadioCardelliSubtyping
  def isWellFormed(stype: SType):Boolean = {
    isClosed(stype) && isWellFormed(Environment.empty[ObjType](),stype)
  }
  def isWellFormed(t:Type):Boolean =  isWellFormed(Environment.empty[ObjType](),t)

  private def isWellFormed(env: Environment[ObjType], t:Type):Boolean = t match{
    case p:PrimType => true
    case TypeVar(x) =>  true
    case obj@ObjType(x,methods)=>
      val newEnv = Environment.extend(env,x.name,obj)
      methods.forall(m => m.mtype.domain.forall(s=> isWellFormed(env,s)) && isWellFormed(env,m.mtype.codomain))
  }

  def closeType(env: Environment[ObjType],t: Type, ):Type =
    if(env.isEmpty) t
    else throw new NotImplementedError("Finish!")

  private def isWellFormed(env: Environment[ObjType], s:SType):Boolean =
    isWellFormed(env,s.privateType)&&isWellFormed(env,s.publicType)&&
      sb.<::(closeType(env,s.privateType),closeType(env,s.publicType)) ||
    {
      errorCollector.report(s"Private facet must be subtype of public facet in security type: ${s}")
      false
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
