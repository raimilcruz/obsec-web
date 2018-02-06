package ObSecG.Static


import Common.{Environment, ErrorCollector}
import ObSecG.Ast._

/**
   Implements well-formed judgments for types.
  */
class WellFormedCheckerG(val errorCollector : ErrorCollector) {
  val sb = new AmadioCardelliSubtypingG
  type Delta = Set[String]
  def isWellFormed(stype: STypeG):Boolean = isWellFormed(stype.privateType) &&  isWellFormed(stype.publicType)

  /**
    * Verifies if a type is well-formed
    * @param t
    * @return
    */
  def isWellFormed(t:TypeG):Boolean = isWellFormed(Set[String](), Environment.empty[ObjectType](),t)

  private def isWellFormed(delta:Delta, env: Environment[ObjectType], t:TypeG):Boolean = t match{
    case p:PrimType => true
    case TypeVar(x) =>  true
    case obj@ObjectType(x,methods)=>
      val newEnv = Environment.extend(env,x,obj)
      if (methods.map(x => x.name).distinct.size != methods.size){
        errorCollector.report("An object type can not have repeated method names")
        false
      }
      else
        methods.forall(m => m.mType.domain.forall(s=> isWellFormed(delta,newEnv,s)) && isWellFormed(delta,newEnv,m.mType.codomain))
  }
  def closeType(env: Environment[ObjectType],t: TypeG):TypeG =
    if(env.isEmpty) t
    else {
      val head= env.head
      closeType(env.tail,TypeSubstG.substSelf(t,head._1,head._2))
    }

  private def isWellFormed(delta: Delta, env: Environment[ObjectType], s:STypeG):Boolean =
    isWellFormed(delta,env,s.privateType)&& isWellFormed(delta,env,s.publicType)&&
      (sb.<::(closeType(env,s.privateType),closeType(env,s.publicType)) ||
        {
          errorCollector.report(s"Private facet must be subtype of public facet in security type: ${s}")
          false
        })

  def isClosed(s: STypeG): Boolean = isSClosed(Environment.empty[Boolean](), s)

  private def isSClosed(environment: Environment[Boolean], s: STypeG): Boolean = {
    isClosed(environment, s.privateType) && isClosed(environment, s.publicType)
  }

  private def isClosed(env: Environment[Boolean], t: TypeG): Boolean = t match {
    case pt: PrimType => true
    case TypeVar(x) =>
      executeSuccesfully(()=> env.lookup(x)) || {
        errorCollector.report(s"Free type variable ${x}")
        false
      }
    case ObjectType(x,methods)=> {
      if(executeSuccesfully(() => env.lookup(x)))false
      val newEnv = Environment.extend(env,x,true)
      methods.forall(m => isSClosed(newEnv,m.mType.codomain) && m.mType.domain.forall(x=> isSClosed(newEnv,x)))
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
