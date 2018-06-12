package ObSecG.Static

import ObSecG.Ast._

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

  def normalizeUnion(unionLabel: UnionLabel):LabelG
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

  override def normalizeUnion(unionLabel: UnionLabel): LabelG = ???
  
  private def normalize(label:LabelG):LabelG = label match{
    case l:LabelVar=>l
    case UnionLabel(l1,l2)=>
      val ln1 = normalize(l1)
      val ln2 = normalize(l2)
      if(!ln1.isInstanceOf[LabelVar] && !ln2.isInstanceOf[LabelVar])
        ObjectType.top
      else UnionLabel(l1,l2)
    case tv:TypeVar=> throw new Error("We should get here! We need to close the type")
    case ObjectType(selfVar,methods) =>
      ObjectType(
        selfVar,
        methods.map(m=>
          MethodDeclarationG(
            m.name,
            MTypeG(
              m.mType.typeVars,
              m.mType.domain.map(s=> STypeG(s.privateType,s.publicType)),
              STypeG(m.mType.codomain.privateType,m.mType.codomain.publicType)
            ))))
      //TODO: finish this.
  }
}