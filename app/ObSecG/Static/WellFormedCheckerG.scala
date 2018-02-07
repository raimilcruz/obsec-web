package ObSecG.Static


import Common.{EmptyEnvironment, Environment, ErrorCollector}
import ObSecG.Ast._

trait IWellFormedCheckerG {
  def isWellFormed(genVarEnv: Environment[TypeG], stype: STypeG): Boolean

  def isWellFormed(stype: STypeG): Boolean
}

/**
  * Implements well-formed judgments for types.
  */
class WellFormedCheckerG(val errorCollector: ErrorCollector)
  extends IWellFormedCheckerG {

  val sb = new AmadioCardelliSubtypingG
  type Delta = Set[String]

  def isWellFormed(stype: STypeG): Boolean =
    isWellFormed(stype.privateType) && isWellFormed(stype.publicType)

  def isWellFormed(genVarEnv: Environment[TypeG],
                   stype: STypeG): Boolean =
    isWellFormed(genVarEnv, stype.privateType) &&
      isWellFormed(genVarEnv, stype.publicType)

  /**
    * Verifies if a type is well-formed
    *
    * @param t
    * @return
    */
  def isWellFormed(t: TypeG): Boolean =
    isWellFormed(
      new EmptyEnvironment[TypeG],
      Set[String](),
      Environment.empty[ObjectType]()
      , t)

  def isWellFormed(genVarEnv: Environment[TypeG], t: TypeG): Boolean =
    isWellFormed(genVarEnv, Set[String](), Environment.empty[ObjectType](), t)


  private def constraintsAreWellFormed(genVarEnv: Environment[TypeG],
                                       delta: Delta,
                                       env: Environment[ObjectType],
                                       typeVars: List[TypeVarSubConstraint]):Boolean = {
    val vars = typeVars.map(x=>x.typeVar)
    if(vars.distinct.size != vars.size) {
      errorCollector.report("There are repeated type variables")
      return false
    }
    if(vars.exists(p => genVarEnv.contains(p))){
      errorCollector.report("Type variable already in scope")
      return false
    }
    typeVariableWellFormed(genVarEnv,delta,env,typeVars)
  }
  private def typeVariableWellFormed(genVarEnv:Environment[TypeG],
                                            delta: Delta,
                                            env: Environment[ObjectType],
                                            typeVars: List[TypeVarSubConstraint]):Boolean= typeVars match{
    case List() => true
    case h::tail =>
      if(!isWellFormed(genVarEnv,delta,env,h.typeBound)){
        errorCollector.report("Type bound is not well-formed")
        return false
      }
      typeVariableWellFormed(
        Environment.extend(genVarEnv,h.typeVar,h.typeBound)
        ,delta,env,tail)
  }

  private def isWellFormed(genVarEnv: Environment[TypeG],
                           delta: Delta,
                           env: Environment[ObjectType],
                           t: TypeG): Boolean = t match {
    case p: PrimType => true
    case TypeVar(x) => true
    case GenericTypeVar(x) =>
      genVarEnv.contains(x)
    case obj@ObjectType(x, methods) =>
      val newEnv = Environment.extend(env, x, obj)
      if (methods.map(x => x.name).distinct.size != methods.size) {
        errorCollector.report("An object type can not have repeated method names")
        false
      }
      else
        methods
          .forall(m =>
            constraintsAreWellFormed(genVarEnv,delta,env,m.mType.typeVars) &&
            m.mType
              .domain
              .forall(s =>
                isWellFormed(
                  Helper.multiExtend(genVarEnv, m.mType.typeVars),
                  delta, newEnv, s)) &&
              isWellFormed(
                Helper.multiExtend(genVarEnv, m.mType.typeVars),
                delta, newEnv, m.mType.codomain))
  }

  def closeType(env: Environment[ObjectType], t: TypeG): TypeG =
    if (env.isEmpty) t
    else {
      val head = env.head
      closeType(env.tail, TypeSubstG.substTypeVar(t, head._1, head._2))
    }

  private def isWellFormed(genVarEnv: Environment[TypeG],
                           delta: Delta,
                           env: Environment[ObjectType],
                           s: STypeG): Boolean =
    isWellFormed(genVarEnv, delta, env, s.privateType) &&
      isWellFormed(genVarEnv, delta, env, s.publicType) &&
      (sb.<::(
        genVarEnv,
        closeType(env, s.privateType),
        closeType(env, s.publicType)) || {
        errorCollector.report(s"Private facet must be subtype of " +
          s"public facet in security type: ${s}")
        false
      })

  def isClosed(s: STypeG): Boolean = isSClosed(Environment.empty[Boolean](), s)

  private def isSClosed(environment: Environment[Boolean], s: STypeG): Boolean = {
    isClosed(environment, s.privateType) && isClosed(environment, s.publicType)
  }

  private def isClosed(env: Environment[Boolean], t: TypeG): Boolean = t match {
    case pt: PrimType => true
    case TypeVar(x) =>
      executeSuccesfully(() => env.lookup(x)) || {
        errorCollector.report(s"Free type variable ${x}")
        false
      }
    case ObjectType(x, methods) =>
      if (executeSuccesfully(() => env.lookup(x))) false
      val newEnv = Environment.extend(env, x, true)
      methods.forall(m => isSClosed(newEnv, m.mType.codomain) && m.mType.domain.forall(x => isSClosed(newEnv, x)))
  }

  private def executeSuccesfully[T](m: () => T): Boolean = {
    try {
      m()
      true
    }
    catch {
      case _: Throwable => false
    }
  }
}
