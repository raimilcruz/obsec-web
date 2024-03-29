package ObSec.Ast

import Common.PrettyPrint
import ObSec.Static.{TypeEquivalence, TypeSubst}

/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class SType(privateType: Type, publicType: Type) {
  def map(f: Type => Type): SType =
    SType(f(privateType), f(publicType))

  override def toString: String ={
    val pString =
      if(TypeEquivalence.alphaEq(publicType,ObjType.top) && !TypeEquivalence.alphaEq(privateType,ObjType.top))"H"
      else if(TypeEquivalence.alphaEq(publicType,privateType)) "L"
      else s"${publicType}"
    s"${privateType}<${pString}"
  }

}

/**
  * Represents an abstract type in ObSec
  * T ::= Obj(X)[mtype ...] | X
  */
trait Type extends PrettyPrint{
  def methSig(x: String): MType

  def containsMethod(x: String): Boolean

  override def prettyPrint(builder:StringBuilder): Unit = builder.append(toString)
}

/**
  * Represents an object recursive type
  *
  * @param typeVar The type variable
  * @param methods The list of method signatures of the object type
  */
case class ObjType(typeVar: TypeVar, methods: List[MethodDeclaration]) extends Type {
  override def methSig(x: String): MType = {
    //we must close the type
    val mt = methods.find(m => m.name == x).get.mtype
    MType(
      mt.domain.map(s => SType(TypeSubst.subst(s.privateType, typeVar.name, this), TypeSubst.subst(s.publicType, typeVar.name, this))),
      SType(TypeSubst.subst(mt.codomain.privateType, typeVar.name, this), TypeSubst.subst(mt.codomain.publicType, typeVar.name, this))
    )
  }

  override def containsMethod(m: String): Boolean = methods.exists(x => x.name == m)

  override def toString: String =
    if(methods.isEmpty)"[]"
    else s"[${typeVar.name} ${methods.map(x => x.toString).fold("")((x: String, y: String) => x + y).toString}]"
}

object ObjType {
  val top = ObjType(TypeVar("x"), List())
}

/**
  * Represents a type variable
  *
  * @param name The variable name
  */
case class TypeVar(name: String) extends Type {
  override def methSig(x: String): MType = throw new Error("Type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Type var does not have methods")

  override def toString: String = name
}



trait PrimType {
  def toObjType: ObjType
}

case object IntType extends Type with PrimType {
  override def methSig(x: String): MType = x match {
    case "+" => MType(List(SType(IntType, IntType)), SType(IntType, IntType))
    case "-" => MType(List(SType(IntType, IntType)), SType(IntType, IntType))
    case "==" => MType(List(SType(IntType, IntType)), SType(BooleanType, BooleanType))
    case _ => throw new Error("Message not understood")
  }

  override def containsMethod(x: String): Boolean = x match {
    case "+" | "-" | "==" => true
    case _ => false
  }

  override def toString: String = "Int"

  override def toObjType(): ObjType =
    ObjType(TypeVar("x"),
      List(
        MethodDeclaration("+",
          MType(
            List(SType(TypeVar("x"), TypeVar("x"))),
            SType(TypeVar("x"), TypeVar("x")))),
        MethodDeclaration("-",
          MType(
            List(SType(TypeVar("x"), TypeVar("x"))),
            SType(TypeVar("x"), TypeVar("x")))),
        MethodDeclaration("==",
          MType(
            List(SType(TypeVar("x"), TypeVar("x"))),
            SType(BooleanType, BooleanType)))
      ))
}

case object StringType extends Type with PrimType {
  override def methSig(method: String): MType = method match {
    case "==" => MType(
      List(SType(StringType, StringType)), SType(BooleanType, BooleanType)
    )
    case "length" => MType(List(), SType(IntType, IntType))
    case "hash" => MType(List(), SType(IntType, IntType))
    case _ => throw new Error(s"Method ${method} not found! ")
  }

  override def containsMethod(x: String): Boolean = x match {
    case "==" | "length" | "hash" => true
    case _ => false
  }

  override def toString: String = "String"

  override def toObjType(): ObjType = ObjType(
    TypeVar("x"),
    List(
      MethodDeclaration("==", MType(
        List(SType(TypeVar("x"), TypeVar("x"))), SType(BooleanType, BooleanType))),
      MethodDeclaration("length", MType(
        List(), SType(IntType, IntType))),
      MethodDeclaration("hash", MType(
        List(), SType(IntType, IntType)))
    )
  )
}

case object BooleanType extends Type with PrimType {
  override def methSig(x: String): MType = throw new NotImplementedError()

  override def containsMethod(x: String): Boolean = false

  override def toString: String = "Bool"

  override def toObjType(): ObjType = ObjType(TypeVar("x"), List(MethodDeclaration("$notarealmethod$",MType(List(),SType(TypeVar("x"),TypeVar("x"))))))

}


case object StringListType extends Type with PrimType{
  override def methSig(x: String): MType = x match{
    case "isEmpty" => MType(List(),SType(BooleanType,BooleanType))
    case "head" => MType(List(),SType(StringType,StringType))
    case "tail" => MType(List(),SType(StringListType,StringListType))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjType = ObjType(TypeVar("x"),
    List(MethodDeclaration("isEmpty",MType(List(),SType(BooleanType,BooleanType))),
      MethodDeclaration("head",MType(List(),SType(StringType,StringType))),
      MethodDeclaration("tail",MType(List(),SType(TypeVar("x"),TypeVar("x"))))
    ))

  override def toString: String = "StrList"
}

/**
  *
  * @param elemPolicy The type must be subtype of String
  */
case class StringGListType(elemPolicy: Type) extends Type with PrimType{
  override def methSig(x: String): MType = x match{
    case "isEmpty" => MType(List(),SType(BooleanType,BooleanType))
    case "head" => MType(List(),SType(StringType,elemPolicy))
    case "tail" => MType(List(),SType(StringGListType(elemPolicy),StringGListType(elemPolicy)))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjType = ObjType(TypeVar("x"),
    List(MethodDeclaration("isEmpty",MType(List(),SType(BooleanType,BooleanType))),
      MethodDeclaration("head",MType(List(),SType(StringType,elemPolicy))),
      MethodDeclaration("tail",MType(List(),SType(TypeVar("x"),TypeVar("x"))))
    ))

  override def toString: String = s"StrList[$elemPolicy]"
}


sealed trait LabelType extends Type {
  override def methSig(x: String): MType = throw new Error("It does not sense")

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
  * @param mtype The method type
  */
case class MethodDeclaration(name: String, mtype: MType) {
  override def toString: String = s"{$name : $mtype }"
}

/**
  * Represents a method type S1 -> S2
  *
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MType(domain: List[SType], codomain: SType) {
  def map(f: SType => SType): MType =
    MType(domain.map(f), f(codomain))

  override def toString: String = s"${domain.foldLeft("")((acc, x) => acc + " " + x)} -> $codomain"
}
