/*
package ObSecE.Ast

import Common.PrettyPrint
import ObSec.Ast.SType
import ObSecG.Static.{TypeEquivalenceG, TypeSubstG}

/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class STypeG(privateType: TypeG, publicType: LabelG) extends GObSecElement with PrettyPrint {
  def map(f: TypeG => TypeG, g: LabelG => LabelG): STypeG =
    STypeG(f(privateType), g(publicType)).setAstNode(this.astNode)

  def map(g: LabelG => LabelG): STypeG =
    STypeG(g(privateType).asInstanceOf[TypeG], g(publicType)).setAstNode(this.astNode)


  /* override def toString: String ={
     val pString: String =
       if(publicType.equals(ParametricObjectType.top) && privateType.equals(ParametricObjectType.top))"H"
       else if(TypeEquivalenceG.alphaEq(publicType,privateType)) "L"
       else s"$publicType"
     s"$privateType<$pString"
   }*/
  override def toString: String = s"ST($privateType,$publicType)"

  override def prettyPrint(buffer:StringBuilder): Unit = {
    privateType.prettyPrint(buffer)
    buffer.append("<")
    publicType.prettyPrint(buffer)
  }
  def prettyPrint(): String = {
    val buffer = new StringBuilder
    privateType.prettyPrint(buffer)
    buffer.append("<")
    publicType.prettyPrint(buffer)
    buffer.toString()
  }
}


trait IObject{
  def methSig(x: String): MTypeE
  def containsMethod(x: String): Boolean
}
trait LabelG extends IObject with PrettyPrint with GObSecElement {
  def prettyPrint():String ={
    val s = new StringBuilder
    prettyPrint(s)
    s.toString()
  }
}

/**
  * Represents a generic method type  <X> S1 -> S2
  *
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MTypeE(domain: List[STypeG], codomain: STypeG) extends PrettyPrint {
  def map(f: STypeG => STypeG): MTypeE =
    MTypeE(domain.map(f), f(codomain))

  override def prettyPrint(buffer: StringBuilder): Unit = {
    domain.foreach(
      st => {
        buffer.append(" ")
        st.prettyPrint(buffer)
      })
    buffer.append("->")
    codomain.prettyPrint(buffer)
  }
}



/**
  * Represents an abstract type in ObSec
  * T ::= O | a | X
  * O ::= Obj(a)[m<X<:T>: S -> S ...]
  */
trait TypeG extends LabelG {

  def unUsedTypeVar: String = "unused"

  //def UnUsedTypeVars: List[BoundedLabelVar] = List(BoundedLabelVar("x",Bottom, ObjectType.top))
}

/*/*
Used to instantiate constraints of the form
X > Int, X > String. Without * the only way is to provide A type that
satisfies both constraint. However with * we can also instantiate usages
of the form Int<X to Int and usages of the form String<X to String.
 */
object Asterisk extends TypeOrAsterisk*/


/**
  * T ::= ... X ...
  * @param name the variable name
  */
case class LabelVar(name: String) extends LabelG{
  override def methSig(x: String): MTypeG =
    throw new Error("It is an error to ask methSig to a label var")

  override def containsMethod(x: String): Boolean = false

  var isAster:Boolean = false
  def setAster(b:Boolean): this.type = {
    isAster = b
    this
  }

  override def prettyPrint(buffer:StringBuilder): Unit =
    buffer.append(name)
}


object Bottom extends LabelG {
  override def methSig(x: String): MTypeG =  throw new Error("methSig over bottom")

  override def containsMethod(x: String): Boolean = false

  override def prettyPrint(buffer:StringBuilder): Unit=
    buffer.append("bot")
}

*/
