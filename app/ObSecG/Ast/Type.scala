package ObSecG.Ast

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



trait LabelG extends PrettyPrint with GObSecElement {
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
  var isAster:Boolean = false
  def setAster(b:Boolean): this.type = {
    isAster = b
    this
  }

  override def prettyPrint(buffer:StringBuilder): Unit =
    buffer.append(name)
}


object Bottom extends LabelG {

  override def prettyPrint(buffer:StringBuilder): Unit=
    buffer.append("bot")
}

/*case class UnionLabel(left: LabelG,right: LabelG) extends LabelG {
    //throw new Error(s"UnionLabel.containsMethod not implemented. Request method: $x to type ${this}")

  override def prettyPrint(buffer:StringBuilder): Unit = {
    buffer.append("(")
    left.prettyPrint(buffer)
    buffer.append(",")
    right.prettyPrint(buffer)
    buffer.append(")")

  }
}*/

trait Primitable{
  var isPrimitive = false
  def setIsPrimitive(b:Boolean):this.type ={
    isPrimitive = b
    this
  }
}
trait IObject{
  def methSig(x: String): MTypeG
  def containsMethod(x: String): Boolean
}
case class ObjectType(selfVar: String, methods: List[MethodDeclarationG]) extends TypeG with IObject{


  override def methSig(x: String): MTypeG = {
    //we must close the type
    val mD = methods.find(m => m.name == x).get
    MTypeG(
      mD.mType.typeVars.map(tv=>{
        BoundedLabelVar(tv.typeVar,
          TypeSubstG.substRecVar(tv.lowerBound,selfVar,this),
          TypeSubstG.substRecVar(tv.upperBound,selfVar,this)
        ).setAster(tv.isAster)
      }),
      mD.mType.domain.map(s =>
        STypeG(
          TypeSubstG.substRecVar(s.privateType, selfVar, this).asInstanceOf[TypeG],
          TypeSubstG.substRecVar(s.publicType, selfVar, this))),
      STypeG(
        TypeSubstG.substRecVar(mD.mType.codomain.privateType, selfVar, this).asInstanceOf[TypeG],
        TypeSubstG.substRecVar(mD.mType.codomain.publicType, selfVar, this))
    )
  }

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
case class TypeVar(name: String) extends TypeG {
  override def toString: String = name

  override def prettyPrint(builder: StringBuilder): Unit =
    builder.append(name)
}

object UnUsedTypeVars {
  def apply: List[BoundedLabelVar] = List(BoundedLabelVar("unused",Bottom, ObjectType.top))
}

trait PrimType extends TypeG{
  def name : String
  def methods: List[MethodDeclarationG]

  def prettyPrint(builder: StringBuilder):Unit=
    builder.append(toString)

  def st : STypeG = STypeG(this,ImplicitLabel)

  override def toString: String = name
}

case object ImplicitLabel extends LabelG {
  override def prettyPrint(buffer: StringBuilder): Unit = buffer.append("I*")

  override def toString: String = "I"

}

//case class SingleMethodSig(name:String, domain:List[PrimType], codomain:PrimType)

object IntADT extends PrimType {
  override def name: String = "Int"
  override def methods: List[MethodDeclarationG] =
    List(
      MethodDeclarationG("+",MTypeG(List(),List(IntADT.st),IntADT.st).setPrimitive(true)),
      MethodDeclarationG("-",MTypeG(List(),List(IntADT.st),IntADT.st).setPrimitive(true)),
      MethodDeclarationG("==",MTypeG(List(),List(IntADT.st), BoolADT.st).setPrimitive(true)))


}
object BoolADT extends PrimType{
  override def name: String = "Bool"
  override def methods: List[MethodDeclarationG] = List(
    MethodDeclarationG("it not possible to it model without generic variables",
      MTypeG(List(),List(BoolADT.st),BoolADT.st).setPrimitive(true))
  )

}

object StringADT extends PrimType{
  override def name: String = "String"
  override def methods: List[MethodDeclarationG] = List(
    MethodDeclarationG("==",MTypeG(List(),List(StringADT.st),BoolADT.st).setPrimitive(true)),
    MethodDeclarationG("length",MTypeG(List(),List(),IntADT.st).setPrimitive(true)),
    MethodDeclarationG("hash",MTypeG(List(),List(),IntADT.st).setPrimitive(true)),
      MethodDeclarationG("first",MTypeG(List(),List(),StringADT.st).setPrimitive(true))
  )

}

/*
trait PrimObjectType extends TypeG with PrimType
case object IntType extends PrimObjectType {

  override def methSig(x: String): MTypeG = x match {
    case "+" => MTypeG(
      List(BoundedLabelVar("l",IntType,ObjectType.top)),
      List(STypeG(IntType, LabelVar("l"))),
      STypeG(IntType, LabelVar("l")))
    case "-" => MTypeG(
      List(BoundedLabelVar("l",IntType,ObjectType.top)),
      List(STypeG(IntType, LabelVar("l"))),
      STypeG(IntType, LabelVar("l")))
    case "==" => MTypeG(
      List(BoundedLabelVar("l",IntType,ObjectType.top).setAster(true)),
      List(STypeG(IntType, LabelVar("l"))),
      STypeG(BooleanType, UnionLabel(BooleanType,LabelVar("l").setAster(true))))
    case _ => throw new Error("Message not understood")
  }

  override def containsMethod(x: String): Boolean = x match {
    case "+" | "-" | "==" => true
    case _ => false
  }

  override def toString: String = "Int"

  override def toObjType: ObjectType =
    ObjectType("x",
      List(
        MethodDeclarationG("+",
          MTypeG(
            List(BoundedLabelVar("l",TypeVar("x"),ObjectType.top)),
            List(STypeG(TypeVar("x"), LabelVar("l"))),
            STypeG(TypeVar("x"), LabelVar("l")))),
        MethodDeclarationG("-",
          MTypeG(
            List(BoundedLabelVar("l",TypeVar("x"),ObjectType.top)),
            List(STypeG(TypeVar("x"), LabelVar("l"))),
            STypeG(TypeVar("x"), LabelVar("l")))),
        MethodDeclarationG("==",
          MTypeG(
            List(BoundedLabelVar("l",TypeVar("x"),ObjectType.top).setAster(true)),
            List(STypeG(TypeVar("x"), LabelVar("l").setAster(true))),
            STypeG(BooleanType, UnionLabel(BooleanType,LabelVar("l").setAster(true)))))
      )).setIsPrimitive(true)
}
*/

/**
  * typedef String{
  * //This is bad, because we know for sure that the join operation
  * //(on the standard subtyping lattice) will be Top, except for other String.
  *
  * bool{this join Po} equals[Po<:Top](Top@Po);
  * int@{this join Pch} indexOf[Pch](int@Pch ch);
  * String@{this join Pstr} concat[Pstr](String@Pstr str);
  * String@{this} replace(char@{this} oldChar, char@{this} newChar);
  * String@{this} toUpperCase();
  * String@{this} trim();
  * //it can throws exception. Exceptions are treated as diverging computation
  * String@{this join PbI join PeI} substring[Pbi,Pei](int beginIndex, int endIndex)
  *
  *
  *
  * //the hash signature does not have sense. this>String, so we do not have
  * //a way to relate it to int.
  * int@{this} hash();
  *
  * We need a label that behaves in the following
  * way:
  * this* :=  if(this == String) int then (this join int).
  * int@{this*} hash();
  *
  * String@{this join other} concatInt[Pi](Int@Pi a);  *
  * }
  */
/*case object StringType extends PrimObjectType {
  override def methSig(method: String): MTypeG = method match {
    case "==" =>
      MTypeG(
        List(BoundedLabelVar("l",StringType,ObjectType.top).setAster(true)),
        List(STypeG(StringType, LabelVar("l").setAster(true))),
        STypeG(BooleanType, UnionLabel(BooleanType,LabelVar("l").setAster(true)))
    )
    case "length" => MTypeG(List(), List(), STypeG(IntType, IntType))
    case "hash" => MTypeG(List(), List(), STypeG(IntType, IntType))
    case _ => throw new Error(s"Method $method not found! ")
  }

  override def containsMethod(x: String): Boolean = x match {
    case "==" | "length" | "hash" => true
    case _ => false
  }

  override def toString: String = "String"

  override def toObjType: ObjectType = ObjectType(
    "x",
    List(
      MethodDeclarationG("==",
        MTypeG(
          List(BoundedLabelVar("l",TypeVar("x"),ObjectType.top).setAster(true)),
          List(STypeG(TypeVar("x"), LabelVar("l").setAster(true))),
          STypeG(BooleanType, UnionLabel(BooleanType,LabelVar("l").setAster(true))))),
      MethodDeclarationG("length", MTypeG(List(),
        List(), STypeG(IntType, IntType))),
      MethodDeclarationG("hash", MTypeG(List(),
        List(), STypeG(IntType, IntType)))
    )
  ).setIsPrimitive(true)
}

case object BooleanType extends PrimObjectType {
  override def methSig(x: String): MTypeG = throw new NotImplementedError("BooleanType.methSig not implemented")

  override def containsMethod(x: String): Boolean = false

  override def toString: String = "Bool"

  override def toObjType: ObjectType =
    ObjectType("x", List(MethodDeclarationG("$notarealmethod$",
      MTypeG(List(), List(), STypeG(TypeVar("x"), TypeVar("x")))))).setIsPrimitive(true)

}*/

trait IBuiltinObject{
  def toObjType : ObjectType
}

/*case object StringListType extends PrimType /*with IObject  with IBuiltinObject*/{
  /*override def methSig(x: String): MTypeG = x match {
    case "isEmpty" => MTypeG(List(), List(), STypeG(BoolADT, BoolADT))
    case "head" => MTypeG(List(), List(), STypeG(StringADT, StringADT))
    case "tail" => MTypeG(List(), List(), STypeG(StringListType, StringListType))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty", MTypeG(List(), List(), STypeG(BoolADT, BoolADT))),
      MethodDeclarationG("head", MTypeG(List(), List(), STypeG(StringADT, StringADT))),
      MethodDeclarationG("tail", MTypeG(List(), List(), STypeG(TypeVar("x"), TypeVar("x"))))
    ))//.setIsPrimitive(true)*/

  override def toString: String = "StrList"

  override def prettyPrint(buffer: StringBuilder): Unit = buffer.append("StrList")

  override def name: String = "StrList"

  override def methods: List[MethodDeclarationG] = List(
    MethodDeclarationG("isEmpty", MTypeG(List(), List(), BoolADT.st).setPrimitive(true)),
    MethodDeclarationG("head", MTypeG(List(), List(), StringADT.st).setPrimitive(true)),
    MethodDeclarationG("tail", MTypeG(List(), List(), StringListType.st).setPrimitive(true))
  )
}*/

/**
  *
  * @param elemPolicy The type must be subtype of String
  */
case class StringGListType(elemPolicy: LabelG) extends TypeG with IObject with IBuiltinObject{
  override def methSig(x: String): MTypeG = x match {
    case "isEmpty" => MTypeG(List(), List(), STypeG(BoolADT, BoolADT))
    case "head" => MTypeG(List(), List(), STypeG(StringADT, elemPolicy))
    case "tail" => MTypeG(List(), List(), STypeG(StringGListType(elemPolicy).asInstanceOf[TypeG], StringGListType(elemPolicy)))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty", MTypeG(List(), List(), STypeG(BoolADT, BoolADT))),
      MethodDeclarationG("head", MTypeG(List(), List(), STypeG(StringADT, elemPolicy))),
      MethodDeclarationG("tail", MTypeG(List(), List(), STypeG(TypeVar("x"), TypeVar("x"))))
    ))//.setIsPrimitive(true)

  override def toString: String = s"StrList[$elemPolicy]"

  override def prettyPrint(buffer: StringBuilder): Unit = buffer.append(s"StrList[${elemPolicy.prettyPrint()}]")
}



/**
  * Represent a method signature
  *
  * @param name  The method label
  * @param mType The method type
  */
case class MethodDeclarationG(name: String, mType: MTypeG)  extends GObSecElement with PrettyPrint {
  var methodNameNode :ObSecGAstNode = NoGObSecNode
  def setMethodNameNode(methodName:SimpleIdentifier):MethodDeclarationG={
    if(methodNameNode eq NoGObSecNode) methodNameNode = methodName
    this
  }

  override def toString: String = {
    val typeParams =
      mType
        .typeVars
        .map(tv => s"${tv.toString}").mkString(",")

    var typeParamsPart = if (typeParams.isEmpty) "" else s"[$typeParams]"

    s"{$name$typeParamsPart :" +
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
  * @param typeVars A list of generic variables constraints:  <X<:T>
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MTypeG(typeVars: List[BoundedLabelVar], domain: List[STypeG], codomain: STypeG) extends PrettyPrint {
  def map(f: STypeG => STypeG): MTypeG =
    MTypeG(typeVars, domain.map(f), f(codomain))

  override def prettyPrint(buffer: StringBuilder): Unit = {
    buffer.append("[")
    typeVars.foreach(tv=> {
      tv.prettyPrint(buffer)
      buffer.append(",")
    })
    buffer.append("]")
    domain.foreach(
      st => {
        buffer.append(" ")
        st.prettyPrint(buffer)
      })
    buffer.append("->")
    codomain.prettyPrint(buffer)
  }

  var isPrimitive :Boolean = false
  def setPrimitive(b:Boolean):MTypeG = {
    isPrimitive = b
    this
  }
  def computedIsPrimitive:Boolean = domain.forall(x=>x.publicType == ImplicitLabel) && codomain.publicType == ImplicitLabel
  def usedImplicitLabels = domain.exists(x=>x.publicType == ImplicitLabel) || codomain.publicType == ImplicitLabel
}

case class BoundedLabelVar(typeVar: String
                               , lowerBound: LabelG
                               , upperBound: LabelG) extends GObSecElement with PrettyPrint {
  def map(function: LabelG => LabelG): BoundedLabelVar =
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


case class TypeVarBounds(lower:LabelG,upper:LabelG) extends PrettyPrint {
  override def prettyPrint(buffer:StringBuilder): Unit = {
    lower.prettyPrint(buffer)
    buffer.append("..")
    upper.prettyPrint(buffer)
  }
}
