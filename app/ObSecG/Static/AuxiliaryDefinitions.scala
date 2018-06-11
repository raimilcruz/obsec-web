package ObSecG.Static

import ObSecG.Ast.{BoundedLabelVar, LabelG, LabelVar}

/**
  * We create this trait to group all the auxiliary definition of the model
  * in the paper.
  */
trait IAuxiliaryFunctions extends Environments {
  /**
    * Computes the upper bound of a type (variable) according the
    * generic type variable environment
    *
    * @param labelVarEnvironment The generic type variable environment
    * @param theType             The type (probably a type variable)
    * @return
    */
  def tUpperBound(labelVarEnvironment: LabelVarEnvironment, theType: LabelG): LabelG

  def multiExtend(labelVarEnvironment: LabelVarEnvironment,
                  vars: List[BoundedLabelVar]): LabelVarEnvironment =

    vars.foldLeft(labelVarEnvironment)((acc, c) => acc.extend(c.typeVar, c.bounds))
}

class AuxiliaryFunctions extends IAuxiliaryFunctions {
  /**
    * Computes the upper bound of a type (variable) according the
    * generic type variable environment
    *
    * @param labelVarEnvironment The generic type variable environment
    * @param theType             The type (probably a type variable)
    * @return
    */
  override def tUpperBound(labelVarEnvironment: LabelVarEnvironment,
                           theType: LabelG): LabelG = theType match {
    case labelVar: LabelVar => tUpperBound(labelVarEnvironment, labelVarEnvironment.lookup(labelVar.name).upper)
    case _ => theType
  }
}