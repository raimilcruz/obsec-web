package ObSecE.Ast

import Common.{AstNode, NoAstNode, PrettyPrint}

/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class STypeE(privateType: TypeE, publicType: LabelE) extends EObSecElement with PrettyPrint {
  def map(f: TypeE => TypeE, g: LabelE => LabelE): STypeE =
    STypeE(f(privateType), g(publicType)).setAstNode(this.astNode)

  def map(g: LabelE => LabelE): STypeE =
    STypeE(g(privateType).asInstanceOf[TypeE], g(publicType)).setAstNode(this.astNode)


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



trait LabelE extends PrettyPrint with EObSecElement {
  def prettyPrint():String ={
    val s = new StringBuilder
    prettyPrint(s)
    s.toString()
  }
}



/**
  * Represents an abstract type in ObSec
  * T ::= O | a | X
  * O ::= Obj(a)[m<X<:T>: S -> S ...]
  */
trait TypeE extends LabelE {
  def unUsedTypeVar: String = "unused"
}

/**
  * T ::= ... X ...
  * @param name the variable name
  */
case class LabelVar(name: String) extends LabelE{
  var isAster:Boolean = false
  def setAster(b:Boolean): this.type = {
    isAster = b
    this
  }

  override def prettyPrint(buffer:StringBuilder): Unit =
    buffer.append(name)
}


object Bottom extends LabelE {

  override def prettyPrint(buffer:StringBuilder): Unit=
    buffer.append("bot")
}

trait Primitable{
  var isPrimitive = false
  def setIsPrimitive(b:Boolean):this.type ={
    isPrimitive = b
    this
  }
}
trait IObject{
  def methSig(x: String): MTypeE
  def containsMethod(x: String): Boolean
}
case class ObjectType(selfVar: String, methods: List[MethodDeclarationE]) extends TypeE with IObject{


  override def methSig(x: String): MTypeE = ???

  override def containsMethod(m: String): Boolean = methods.exists(x => x.name == m)

  override def toString: String = s"OT($selfVar,$methods)"

  override def prettyPrint(builder: StringBuilder): Unit =
    if(methods.isEmpty) builder.append("Top")
    else {
      builder.append(s"Obj($selfVar)")
      builder.append("[")
      methods.map(e=> {
        builder.append("\n")
        e.prettyPrint(builder)
      })
      builder.append("]")
    }

}


object ObjectType {
  val top = ObjectType("x", List())
}


/**
  * Represents a type variable
  *
  * @param name The variable name
  */
case class TypeVar(name: String) extends TypeE {
  override def toString: String = name

  override def prettyPrint(builder: StringBuilder): Unit =
    builder.append(name)
}

trait PrimType extends TypeE{
  def name : String
  def methods: List[MethodDeclarationE]

  def prettyPrint(builder: StringBuilder):Unit=
    builder.append(toString)

  def st : STypeE = STypeE(this,ImplicitLabel)

  override def toString: String = name
}

case object ImplicitLabel extends LabelE {
  override def prettyPrint(buffer: StringBuilder): Unit = buffer.append("I*")

  override def toString: String = "I"

}

//case class SingleMethodSig(name:String, domain:List[PrimType], codomain:PrimType)

object IntADT extends PrimType {
  override def name: String = "Int"
  override def methods: List[MethodDeclarationE] =
    List(
      MethodDeclarationE("+",MTypeE(List(IntADT.st),IntADT.st).setPrimitive(true)),
      MethodDeclarationE("-",MTypeE(List(IntADT.st),IntADT.st).setPrimitive(true)),
      MethodDeclarationE("==",MTypeE(List(IntADT.st), BoolADT.st).setPrimitive(true)))


}
object BoolADT extends PrimType{
  override def name: String = "Bool"
  override def methods: List[MethodDeclarationE] = List(
    MethodDeclarationE("it not possible to it model without generic variables",
      MTypeE(List(BoolADT.st),BoolADT.st).setPrimitive(true))
  )

}

object StringADT extends PrimType{
  override def name: String = "String"
  override def methods: List[MethodDeclarationE] = List(
    MethodDeclarationE("==",MTypeE(List(StringADT.st),BoolADT.st).setPrimitive(true)),
    MethodDeclarationE("length",MTypeE(List(),IntADT.st).setPrimitive(true)),
    MethodDeclarationE("hash",MTypeE(List(),IntADT.st).setPrimitive(true))
  )

}

trait IBuiltinObject{
  def toObjType : ObjectType
}

/**
  * Represent a method signature
  *
  * @param name  The method label
  * @param mType The method type
  */
case class MethodDeclarationE(name: String, mType: MTypeE)  extends EObSecElement with PrettyPrint {
  var methodNameNode :AstNode = NoAstNode
  def setMethodNameNode(methodName:SimpleIdentifier):MethodDeclarationE={
    if(methodNameNode eq NoAstNode) methodNameNode = methodName
    this
  }

  override def toString: String = {

    s"{$name:" +
      s"${
        mType
          .domain
          .foldLeft("")(
            (acc, x) =>
              acc + " " + x)
      } -> ${mType.codomain}}"
  }

  override def prettyPrint(buffer:StringBuilder): Unit =  {
    buffer.append("{")
    buffer.append(name)
    mType.prettyPrint(buffer)
    buffer.append("}")
  }
}

/**
  * Represents a generic method type  <X> S1 -> S2
  *
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MTypeE(domain: List[STypeE], codomain: STypeE) extends PrettyPrint {
  def map(f: STypeE => STypeE): MTypeE =
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

  var isPrimitive :Boolean = false
  def setPrimitive(b:Boolean):MTypeE = {
    isPrimitive = b
    this
  }
  def computedIsPrimitive:Boolean = domain.forall(x=>x.publicType == ImplicitLabel) && codomain.publicType == ImplicitLabel
  def usedImplicitLabels = domain.exists(x=>x.publicType == ImplicitLabel) || codomain.publicType == ImplicitLabel
}

case class BoundedLabelVar(typeVar: String
                           , lowerBound: LabelE
                           , upperBound: LabelE) extends EObSecElement with PrettyPrint {
  def map(function: LabelE => LabelE): BoundedLabelVar =
    BoundedLabelVar(typeVar,function(lowerBound),function(upperBound))
      .setAstNode(this.astNode).setAster(this.isAster)

  override def toString: String = s"$typeVar :$lowerBound..$upperBound"
  def bounds: TypeVarBounds = TypeVarBounds(lowerBound,upperBound)


  var isAster :Boolean = false
  def setAster(b:Boolean):BoundedLabelVar = {
    isAster = b
    this
  }

  def rename(name:String):BoundedLabelVar =
    BoundedLabelVar(name,lowerBound,upperBound).setAstNode(astNode).setAster(isAster)

  override def prettyPrint(buffer:StringBuilder): Unit = {
    buffer.append(typeVar)
    buffer.append(": ")
    lowerBound.prettyPrint(buffer)
    buffer.append("..")
    upperBound.prettyPrint(buffer)
  }
}


case class TypeVarBounds(lower:LabelE,upper:LabelE) extends PrettyPrint {
  override def prettyPrint(buffer:StringBuilder): Unit = {
    lower.prettyPrint(buffer)
    buffer.append("..")
    upper.prettyPrint(buffer)
  }
}
