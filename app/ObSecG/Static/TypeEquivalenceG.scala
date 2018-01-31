package ObSecG.Static

import ObSecG.Ast.{STypeG, TypeG, TypeVar}

/**
  * Implements type equivalence for ObSecG.
  */
object TypeEquivalenceG {
  /**
    * Alpha equivalence for types
    * @param t1
    * @param t2
    * @return
    */
  def alphaEq(t1:TypeG,t2:TypeG):Boolean = throw new Error("Not implemented!")

  /**
    * Alpha equivalence for security types
    * @param s1
    * @param s2
    * @return
    */
  def alphaEq(s1:STypeG,s2:STypeG):Boolean = alphaEq(s1.privateType,s2.privateType) && alphaEq(s1.publicType,s2.publicType)



}

