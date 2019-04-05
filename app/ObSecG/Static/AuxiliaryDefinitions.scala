package ObSecG.Static

import Common.Environment
import ObSecG.Ast._

/**
  * Groups all the auxiliary definition of the model in the paper.
  */
trait IAuxiliaryFunctions extends Environments {
  /**
    * Returns the signatures of the method `name` from the type `theType`
    * @param labelVarEnvironment The type variable environment
    * @param theType The type
    * @param name
    * @return
    */
  def mSig(labelVarEnvironment: LabelVarEnvironment, theType: LabelG, name: String):MTypeG
  def mInU(labelVarEnvironment: LabelVarEnvironment,m: String, theType:LabelG): Boolean

  //shortcuts
  def mSig(theType: LabelG, name: String):MTypeG =
    mSig(Environment.empty[TypeVarBounds](),theType,name)
  def mInU(m: String, theType:LabelG): Boolean =
    mInU(Environment.empty[TypeVarBounds](),m,theType)

  /**
    * Computes the upper bound of a type (variable) according the
    * generic type variable environment
    *
    * @param labelVarEnvironment The generic type variable environment
    * @param theType             The type (probably a type variable)
    * @return
    */
  def tUpperBound(labelVarEnvironment: LabelVarEnvironment, theType: LabelG): LabelG

  def tLowerBound(labelVarEnvironment: LabelVarEnvironment, theType: LabelG): LabelG

  def multiExtend(labelVarEnvironment: LabelVarEnvironment,
                  vars: List[BoundedLabelVar]): LabelVarEnvironment =

    vars.foldLeft(labelVarEnvironment)((acc, c) => acc.extend(c.typeVar, c.bounds))

  /**
    * Remove
    * @param unionLabel
    * @param labelVar
    * @return
    */
  def normalize(unionLabel: LabelG, labelVar: BoundedLabelVar):LabelG

  def substRecVar(t: LabelG, x: String, t2: LabelG): LabelG
  def substLabelVar(t: LabelG,x: BoundedLabelVar, t2: LabelG): LabelG
}

class AuxiliaryFunctions extends IAuxiliaryFunctions {

  def mSig(labelVarEnvironment: LabelVarEnvironment, theType: LabelG, methodName: String): MTypeG = theType match{
    case o: ObjectType => o.methSig(methodName)
    case p: PrimType =>
      println(s"!!!! Method $methodName in label $theType")
      p.methods.find(method => method.name == methodName).get.mType
    case l:LabelVar => mSig(labelVarEnvironment,tUpperBound(labelVarEnvironment,theType),methodName)
    case _ => throw new NotImplementedError(s"m in U, unsupported case for $theType")
  }
  def mInU(labelVarEnvironment: LabelVarEnvironment,m: String, theType:LabelG): Boolean = theType match{
    case p: PrimType=> p.methods.count(sig => sig.name == m) >=0
    case ot: ObjectType => ot.containsMethod(m)
    case l:LabelVar => mInU(labelVarEnvironment,m,tUpperBound(labelVarEnvironment,theType))
    case _ => throw new NotImplementedError(s"m in U, unsupported case for $theType")
  }


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

  override def tLowerBound(labelVarEnvironment: LabelVarEnvironment,
                           theType: LabelG): LabelG = theType match {
    case labelVar: LabelVar => tLowerBound(labelVarEnvironment, labelVarEnvironment.lookup(labelVar.name).lower)
    case _ => theType
  }

  override def normalize(label: LabelG, labelVar: BoundedLabelVar): LabelG = label match{
    case l:LabelVar=>
      if(labelVar.typeVar == l.name && labelVar.isAster) labelVar.lowerBound
      else l
    /*case UnionLabel(l1,l2)=>
      (l1,l2) match{
        case (lv1:LabelVar,lv2:LabelVar)
          if lv1.isAster && lv2.isAster &&
            lv1.name == labelVar.typeVar && lv2.name == labelVar.typeVar =>
          //lift the X*
          lv1
        case (lv1:LabelVar,_) if lv1.isAster && lv1.name == labelVar.typeVar =>
          normalize(l2,labelVar)
        case (_,lv2:LabelVar) if lv2.isAster && lv2.name == labelVar.typeVar =>
          normalize(l1,labelVar)
        case _ =>
          val ln1 = normalize(l1,labelVar)
          val ln2 = normalize(l2,labelVar)
          //stop condition: both types are
          ln1 match {
            case labelVar1: LabelVar if labelVar1.name == labelVar.typeVar => ln2
            case _ => ln2 match {
              case labelVar1: LabelVar if labelVar1.name == labelVar.typeVar => ln1
              case _ => UnionLabel(ln1, ln2)
            }
          }
      }*/
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
    case p:PrimType => p
  }
  override def substRecVar(t: LabelG, x: String, t2: LabelG): LabelG = {
    val typeSubstitutions = new TypeSubstG(this)
    typeSubstitutions.substRecVar(t,x,t2)
  }
  override def substLabelVar(t: LabelG, x: BoundedLabelVar, t2: LabelG): LabelG ={
    val typeSubstitutions = new TypeSubstG(this)
    typeSubstitutions.substLabel(t,x,t2)
  }
}