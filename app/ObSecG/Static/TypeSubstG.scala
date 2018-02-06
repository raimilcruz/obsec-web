package ObSecG.Static

import ObSecG.Ast._

/**
  * Implements avoiding capture substitution for ObSecG.
  */
object TypeSubstG {
  def substSelf(t: TypeG, x: String, t2: TypeG): TypeG = throw new Error("Not implemented")
  def substTypeVar(t: ObjectType, x: String, t2: TypeG): TypeG = throw new Error("Not implemented")

  def subst(t: TypeG, x: String, t2: TypeG): TypeG = throw  new Error("Not implemented")
}
