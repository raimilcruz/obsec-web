package ObSecG.Static


import Common.{EmptyEnvironment, Environment, ErrorCollector}
import ObSecG.Ast._

abstract class IWellFormedCheckerG(judgements: GObSecJudgements, errors: ErrorCollector)
  extends IJudgment(judgements, errors){

  def isWellFormed(genVarEnv: LabelVarEnvironment, stype: STypeG): Boolean

  def isWellFormed(stype: STypeG): Boolean
}

/**
  * Implements well-formed judgments for types.
  */
class WellFormedCheckerG(judgements: GObSecJudgements, errorCollector: ErrorCollector)
  extends IWellFormedCheckerG(judgements,errorCollector) {

  def isWellFormed(stype: STypeG): Boolean =
    //isWellFormed(stype.privateType) && isWellFormed(stype.publicType)
  throw new NotImplementedError()

  def isWellFormed(genVarEnv: LabelVarEnvironment,
                   stype: STypeG): Boolean =
    //isWellFormed(genVarEnv, stype.privateType) &&
     // isWellFormed(genVarEnv, stype.publicType)
    throw new NotImplementedError()



  def isWellFormed(labelVariableEnv: LabelVarEnvironment, t: LabelG): Boolean =
    //isWellFormed(labelVariableEnv, Set[String](), Environment.empty[ObjectType](), t)
  throw new NotImplementedError()


  /* def isWellFormed(t: LabelG): Boolean =
    isWellFormed(
      Environment.empty(),
      Set[String](),
      Environment.empty[ObjectType]()
      , t)



    */

   private def isWellFormed(genVarEnv: LabelVarEnvironment,
                             objectEnv: SelfDefinitionEnvironment,
                             t: Label): Boolean = t match {
      case p: PrimType => true
      case TypeVar(x) => true
      case LabelVarImpl(x) => true
      case obj@ObjectType(x, methods) =>
        val newEnv = Environment.extend(objectEnv, x, obj)
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

  private def typeVariableWellFormed(genVarEnv: LabelVarEnvironment,
                                     objectEnv: SelfDefinitionEnvironment,
                                     typeVars: List[TypeVarBounds]):Boolean= typeVars match{
    case List() => true
    case h::tail =>
      if(!isWellFormed(genVarEnv,objectEnv,h.lower)){
        errorCollector.report("Type bound is not well-formed")
        return false
      }
      typeVariableWellFormed(
        Environment.extend(genVarEnv,h.typeVar,h.typeBound)
        ,delta,env,tail)
  }
  /*
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
     }*/
}
