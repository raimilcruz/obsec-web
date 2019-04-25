package ObSecE.Static

import Common.Environment
import ObSecE.Ast._

import scala.collection.mutable

/**
  * Groups all the auxiliary definition of the model in the paper.
  */
trait IAuxiliaryFunctions{
  def mSig(theType: LabelE, name: String):MTypeE
  def mInU(m: String, theType:LabelE): Boolean

  def multiExtend[T](labelVarEnvironment:  Environment[T],
                     mappings: List[(String,T)]):  Environment[T] =

    mappings.foldLeft(labelVarEnvironment)((acc, assoc) => acc.extend(assoc._1, assoc._2))

  //subst both existential and self variables
  def subst(t: LabelE, x: String, t2: LabelE,labelSubstitution:mutable.HashMap[String,String]): LabelE
}

class AuxiliaryFunctions extends IAuxiliaryFunctions {

  def mSig(theType: LabelE, methodName: String): MTypeE = theType match{
    case o: ObjectType => o.methSig(methodName)
    case et:ExistentialType=> et.methods.find(methDecl => methDecl.name == methodName).get.mType
    case p: PrimType =>
      println(s"!!!! Method $methodName in label $theType")
      p.methods.find(method => method.name == methodName).get.mType
    case _ => throw new NotImplementedError(s"mSig, unsupported case for $theType")
  }
  def mInU(m: String, theType:LabelE): Boolean = theType match{
    case p: PrimType=> p.methods.count(sig => sig.name == m) >=0
    case ot: ObjectType => ot.containsMethod(m)
    case l:LabelVar => false
    case et:ExistentialType=> et.methods.exists(methDecl => methDecl.name == m)
    case _ => throw new NotImplementedError(s"m in U, unsupported case for $theType")

  }

  override def subst(t: LabelE, x: String, t2: LabelE,labelSubstitution:mutable.HashMap[String,String]): LabelE = {
    val typeSubstitutions = new TypeSubstE(this)
    typeSubstitutions.subst(t,x,t2,labelSubstitution)
  }
}