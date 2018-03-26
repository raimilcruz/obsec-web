package ObSecG.Ast

import ObSec.Ast.SType
import ObSecG.Static.{TypeEquivalenceG, TypeSubstG}

/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class STypeG(privateType: TypeG, publicType: TypeG) {
  def map(f: TypeG => TypeG): STypeG =
    STypeG(f(privateType), f(publicType))

 /* override def toString: String ={
    val pString: String =
      if(publicType.equals(ParametricObjectType.top) && privateType.equals(ParametricObjectType.top))"H"
      else if(TypeEquivalenceG.alphaEq(publicType,privateType)) "L"
      else s"$publicType"
    s"$privateType<$pString"
  }*/
  override def toString: String = s"ST($privateType,$publicType)"
}

trait TypeOrAsterisk

trait LabelG extends TypeOrAsterisk{
  def methSig(x: String): MTypeG
  def containsMethod(x: String): Boolean
}

/**
  * Represents an abstract type in ObSec
  * T ::= O | a | X
  * O ::= Obj(a)[m<X<:T>: S -> S ...]
  */
trait TypeG extends LabelG{

  def unUsedTypeVar:String = "unused"
  def UnUsedTypeVars:List[TypeVarSub]= List(TypeVarSub("x",ObjectType.top))
}

/*
Used to instantiate constraints of the form
X > Int, X > String. Without * the only way is to provide A type that
satisfies both constraint. However with * we can also instantiate usages
of the form Int<X to Int and usages of the form String<X to String.
 */
object Asterisk extends  TypeOrAsterisk

/**
  * The self label statically does not provide any method.
  */
object SelfLabel extends LabelG{
  override def methSig(x: String): MTypeG =
    throw new Error("It is an error to ask methSig to the self label")

  override def containsMethod(x: String): Boolean = false
}
object LowSelfLabel extends LabelG{
  override def methSig(x: String): MTypeG =
    throw new Error("It is an error to ask methSig to the low self label")

  override def containsMethod(x: String): Boolean = false
}
object BoundGenericLabel extends LabelG{
  override def methSig(x: String): MTypeG =
    throw new Error("It is an error to ask methSig to the low self label")

  override def containsMethod(x: String): Boolean = false
}

case class ObjectType(selfVar: String, methods: List[MethodDeclarationG]) extends TypeG {
  override def methSig(x: String): MTypeG = {
    //we must close the type
    val mD = methods.find(m => m.name == x).get
    MTypeG(
      mD.mType.typeVars,
      mD.mType.domain.map(s => STypeG(TypeSubstG.substTypeVar(s.privateType,selfVar, this),
        TypeSubstG.substTypeVar(s.publicType, selfVar, this))),
      STypeG(TypeSubstG.substTypeVar(mD.mType.codomain.privateType, selfVar, this),
        TypeSubstG.substTypeVar(mD.mType.codomain.publicType, selfVar, this))
    )
  }

  override def containsMethod(m: String): Boolean = methods.exists(x => x.name == m)

  override def toString: String = s"OT($selfVar,$methods)"
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
  override def methSig(x: String): MTypeG = throw new Error("Type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Type var does not have methods")

  override def toString: String = name
}

case class GenericTypeVar(name:String) extends TypeG{
  override def methSig(x: String): MTypeG = throw new Error("Generic type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Generic Type var does not have methods")

  override def toString: String = s"GV($name)"
}
/*case class TypeVarG(name: String) extends TypeG {
  override def methSig(x: String): MTypeG = throw new Error("Type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Type var does not have methods")

  override def toString: String = name
}*/

object UnUsedTypeVars {
  def apply:List[TypeVarSub] = List(TypeVarSub("unused",ObjectType.top))
}

trait PrimType {
  def toObjType: ObjectType
}

case object IntType extends TypeG with PrimType {

  override def methSig(x: String): MTypeG = x match {
    case "+" => MTypeG(List(),List(STypeG(IntType, IntType)), STypeG(IntType, IntType))
    case "-" => MTypeG(List(), List(STypeG(IntType, IntType)), STypeG(IntType, IntType))
    case "==" => MTypeG(List(),List(STypeG(IntType, IntType)), STypeG(BooleanType, BooleanType))
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
            List(),
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(TypeVar("x"), TypeVar("x")))),
        MethodDeclarationG("-",
          MTypeG(
            List(),
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(TypeVar("x"), TypeVar("x")))),
        MethodDeclarationG("==",
          MTypeG(
            List(),
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(BooleanType, BooleanType)))
      ))
}

/**
  * typedef String{
  *   //This is bad, because we know for sure that the join operation
  *   //(on the standard subtyping lattice) will be Top, excepto for other String.
  *
  *   bool{this join Po} equals[Po](Top@Po);
  *   int@{this join Pch} indexOf[Pch](int@Pch ch);
  *   String@{this join Pstr} concat[Pstr](String@Pstr str);
  *   String@{this} replace(char@{this} oldChar, char@{this} newChar);
  *   String@{this} toUpperCase();
  *   String@{this} trim();
  *   //it can throws exception. Exceptions are treated as diverging computation
  *   String@{this join PbI join PeI} substring[Pbi,Pei](int beginIndex, int endIndex)
  *
  *
  *
  *   //the hash signature does not have sense. this>String, so we do not have
  *   //a way to relate it to int.
  *   int@{this} hash();
  *
  *   We need a label that behaves in the following
  *   way:
  *   this* :=  if(this == String) int then (this join int).
  *   int@{this*} hash();
  *
  *   String@{this join other} concatInt[Pi](Int@Pi a);  *
  * }
  */
case object StringType extends TypeG with PrimType {
  override def methSig(method: String): MTypeG = method match {
    case "==" => MTypeG(List(),
      List(STypeG(StringType, StringType)), STypeG(BooleanType, BooleanType)
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
      MethodDeclarationG("==", MTypeG(List(),
        List(STypeG(TypeVar("x"), TypeVar("x"))), STypeG(BooleanType, BooleanType))),
      MethodDeclarationG("length", MTypeG(List(),
        List(), STypeG(IntType, IntType))),
      MethodDeclarationG("hash", MTypeG(List(),
        List(), STypeG(IntType, IntType)))
    )
  )
}

case object BooleanType extends TypeG with PrimType {
  override def methSig(x: String): MTypeG = throw new NotImplementedError()

  override def containsMethod(x: String): Boolean = false

  override def toString: String = "Bool"

  override def toObjType: ObjectType =
    ObjectType("x", List(MethodDeclarationG("$notarealmethod$",
              MTypeG(List(), List(),STypeG(TypeVar("x"),TypeVar("x"))))))

}


case object StringListType extends TypeG with PrimType{
  override def methSig(x: String): MTypeG = x match{
    case "isEmpty" => MTypeG(List(), List(),STypeG(BooleanType,BooleanType))
    case "head" => MTypeG(List(),List(),STypeG(StringType,StringType))
    case "tail" => MTypeG(List(),List(),STypeG(StringListType,StringListType))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty",MTypeG(List(), List(),STypeG(BooleanType,BooleanType))),
      MethodDeclarationG("head",MTypeG(List(),List(),STypeG(StringType,StringType))),
      MethodDeclarationG("tail",MTypeG(List(),List(),STypeG(TypeVar("x"),TypeVar("x"))))
    ))

  override def toString: String = "StrList"
}

/**
  *
  * @param elemPolicy The type must be subtype of String
  */
case class StringGListType(elemPolicy: TypeG) extends TypeG with PrimType{
  override def methSig(x: String): MTypeG = x match{
    case "isEmpty" => MTypeG(UnUsedTypeVars, List(),STypeG(BooleanType,BooleanType))
    case "head" => MTypeG(UnUsedTypeVars,List(),STypeG(StringType,elemPolicy))
    case "tail" => MTypeG(UnUsedTypeVars,List(),STypeG(StringGListType(elemPolicy),StringGListType(elemPolicy)))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty",MTypeG(UnUsedTypeVars, List(),STypeG(BooleanType,BooleanType))),
      MethodDeclarationG("head",MTypeG(UnUsedTypeVars,List(),STypeG(StringType,elemPolicy))),
      MethodDeclarationG("tail",MTypeG(UnUsedTypeVars,List(),STypeG(TypeVar("x"),TypeVar("x"))))
    ))

  override def toString: String = s"StrList[$elemPolicy]"
}


sealed trait LabelType extends TypeG {
  override def methSig(x: String): MTypeG = throw new Error("It does not sense")

  override def containsMethod(x: String): Boolean = throw new Error("It does not sense")
}

case object LowLabel extends LabelType {
  override def toString: String = "L"
}

case object HighLabel extends LabelType {
  override def toString: String = "H"
}

/**
  * Represent a method signature
  *
  * @param name  The method label
  * @param mType The method type
  */
case class MethodDeclarationG(name: String, mType: MTypeG) {
  override def toString: String = {
    val typeParams =
      mType
        .typeVars
        .map(tv => s"${tv.toString}").mkString(",")

    var typeParamsPart = if(typeParams.isEmpty)"" else s"[$typeParams]"

    s"{$name$typeParamsPart :" +
      s"${
        mType
          .domain
          .foldLeft("")(
            (acc, x) =>
              acc + " " + x)
      } -> ${mType.codomain}}"
  }
}

/**
  * Represents a generic method type  <X> S1 -> S2
  *
  * @param typeVars A list of generic variables constraints:  <X<:T>
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MTypeG(typeVars : List[TypeVarSubConstraint], domain: List[STypeG], codomain: STypeG) {
  def map(f: STypeG => STypeG): MTypeG =
    MTypeG(typeVars,domain.map(f), f(codomain))
}
trait TypeVarSubConstraint{
  def typeVar : String
  def typeBound :TypeG
}
case class TypeVarSub(x:String,upperBound:TypeG) extends TypeVarSubConstraint{
  override def toString: String = s"$x<:$upperBound"

  override def typeVar: String = x

  override def typeBound: TypeG = upperBound
}
case class TypeVarSuper(lowerBound:TypeG,x:String)extends TypeVarSubConstraint{
  override def toString: String = s"$lowerBound<:$x"

  override def typeVar: String = x

  override def typeBound: TypeG = lowerBound
}
