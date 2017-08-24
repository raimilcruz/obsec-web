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

}

/**
  * Represents an abstract type in ObSec
  * T ::= GO<T> | O | a | X
  * GO ::= forall[X <:T].O
  * O ::= Obj(a)[m<X<:T>: S -> S ...]
  */
trait TypeG {
  def methSig(x: String): MTypeG

  def containsMethod(x: String): Boolean

  def unUsedTypeVar:String = "unused"
  def UnusedTypeVarG:String = "unused"
}

/**
  * We treat the Object type of the language as an special guest (following FJ guidelines)
  */
object ObjectG extends TypeG{
  override def methSig(x: String): MTypeG = throw new Error("object does not contain methods")
  override def containsMethod(x: String): Boolean = false
}

/**
  *
  * @param typeVar
  * @param objectType
  */
case class ParametricObjectType(typeVar: String, objectType: ObjectType){
  def instantiate(t : TypeG):TypeG = TypeSubstG.substTypeVar(objectType,typeVar,t)
}

object ParametricObjectType {
  val top = ObjectG
}

case class ObjectType(selfVar: String, methods: List[MethodDeclarationG]) extends TypeG {
  override def methSig(x: String): MTypeG = {
    //we must close the type
    val mD = methods.find(m => m.name == x).get
    MTypeG(
      mD.mType.typeVar,
      mD.mType.typeVarBound,
      mD.mType.domain.map(s => STypeG(TypeSubstG.substSelf(s.privateType,selfVar, this),
        TypeSubstG.substSelf(s.publicType, selfVar, this))),
      STypeG(TypeSubstG.substSelf(mD.mType.codomain.privateType, selfVar, this),
        TypeSubstG.substSelf(mD.mType.codomain.publicType, selfVar, this))
    )
  }

  override def containsMethod(m: String): Boolean = methods.exists(x => x.name == m)
}
object ObjectType {
  val top = ObjectType("x", List())
}

/**
  * Represents to GO<T>
  * @param objectType
  * @param actualType
  */
case class ObjTypeInst(objectType : ParametricObjectType, actualType: TypeG) extends TypeG {
  override def methSig(x: String): MTypeG = objectType.instantiate(actualType).methSig(x)

  override def containsMethod(m: String): Boolean = objectType.instantiate(actualType).containsMethod(m)

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
/*case class TypeVarG(name: String) extends TypeG {
  override def methSig(x: String): MTypeG = throw new Error("Type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Type var does not have methods")

  override def toString: String = name
}*/
object UnusedTypeVarG{
  def apply:String=  "unused"
}

trait PrimType {
  def toObjType: ObjectType
}

case object IntType extends TypeG with PrimType {
  override def methSig(x: String): MTypeG = x match {
    case "+" => MTypeG(unUsedTypeVar, ObjectType.top,List(STypeG(IntType, IntType)), STypeG(IntType, IntType))
    case "-" => MTypeG(unUsedTypeVar,ObjectType.top, List(STypeG(IntType, IntType)), STypeG(IntType, IntType))
    case "==" => MTypeG(UnusedTypeVarG,ObjectType.top,List(STypeG(IntType, IntType)), STypeG(BooleanType, BooleanType))
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
            UnusedTypeVarG,
            ObjectType.top,
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(TypeVar("x"), TypeVar("x")))),
        MethodDeclarationG("-",
          MTypeG(
            UnusedTypeVarG,
            ObjectType.top,
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(TypeVar("x"), TypeVar("x")))),
        MethodDeclarationG("==",
          MTypeG(
            UnusedTypeVarG,
            ObjectType.top,
            List(STypeG(TypeVar("x"), TypeVar("x"))),
            STypeG(BooleanType, BooleanType)))
      ))
}

case object StringType extends TypeG with PrimType {
  override def methSig(method: String): MTypeG = method match {
    case "==" => MTypeG(
      UnusedTypeVarG,ObjectType.top,
      List(STypeG(StringType, StringType)), STypeG(BooleanType, BooleanType)
    )
    case "length" => MTypeG(UnusedTypeVarG,ObjectType.top, List(), STypeG(IntType, IntType))
    case "hash" => MTypeG(UnusedTypeVarG,ObjectType.top, List(), STypeG(IntType, IntType))
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
      MethodDeclarationG("==", MTypeG(UnusedTypeVarG,ObjectType.top,
        List(STypeG(TypeVar("x"), TypeVar("x"))), STypeG(BooleanType, BooleanType))),
      MethodDeclarationG("length", MTypeG(UnusedTypeVarG,ObjectType.top,
        List(), STypeG(IntType, IntType))),
      MethodDeclarationG("hash", MTypeG(UnusedTypeVarG,ObjectType.top,
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
              MTypeG(UnusedTypeVarG,ObjectType.top, List(),STypeG(TypeVar("x"),TypeVar("x"))))))

}


case object StringListType extends TypeG with PrimType{
  override def methSig(x: String): MTypeG = x match{
    case "isEmpty" => MTypeG(unUsedTypeVar,ObjectType.top, List(),STypeG(BooleanType,BooleanType))
    case "head" => MTypeG(unUsedTypeVar,ObjectType.top,List(),STypeG(StringType,StringType))
    case "tail" => MTypeG(unUsedTypeVar,ObjectType.top,List(),STypeG(StringListType,StringListType))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty",MTypeG(unUsedTypeVar,ObjectType.top, List(),STypeG(BooleanType,BooleanType))),
      MethodDeclarationG("head",MTypeG(unUsedTypeVar,ObjectType.top,List(),STypeG(StringType,StringType))),
      MethodDeclarationG("tail",MTypeG(unUsedTypeVar,ObjectType.top,List(),STypeG(TypeVar("x"),TypeVar("x"))))
    ))

  override def toString: String = "StrList"
}

/**
  *
  * @param elemPolicy The type must be subtype of String
  */
case class StringGListType(elemPolicy: TypeG) extends TypeG with PrimType{
  override def methSig(x: String): MTypeG = x match{
    case "isEmpty" => MTypeG(UnusedTypeVarG,ObjectType.top, List(),STypeG(BooleanType,BooleanType))
    case "head" => MTypeG(UnusedTypeVarG,ObjectType.top,List(),STypeG(StringType,elemPolicy))
    case "tail" => MTypeG(UnusedTypeVarG,ObjectType.top,List(),STypeG(StringGListType(elemPolicy),StringGListType(elemPolicy)))
  }

  override def containsMethod(x: String): Boolean = x match {
    case "isEmpty" | "head" | "tail" => true
    case _ => false
  }

  override def toObjType: ObjectType = ObjectType("x",
    List(MethodDeclarationG("isEmpty",MTypeG(UnusedTypeVarG,ObjectType.top, List(),STypeG(BooleanType,BooleanType))),
      MethodDeclarationG("head",MTypeG(UnusedTypeVarG,ObjectType.top,List(),STypeG(StringType,elemPolicy))),
      MethodDeclarationG("tail",MTypeG(UnusedTypeVarG,ObjectType.top,List(),STypeG(TypeVar("x"),TypeVar("x"))))
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
  override def toString: String = s"{$name[${mType.typeVar} <: ${mType.typeVarBound}}] :}" +
    s"${mType.domain.foldLeft("")((acc, x) => acc + " " + x)} -> ${mType.codomain}"
}

/**
  * Represents a generic method type  <X> S1 -> S2
  *
  * @param typeVar the generic type variable of the method.
  * @param domain   The domain type
  * @param codomain The codomain type
  */
case class MTypeG(typeVar : String, typeVarBound : TypeG, domain: List[STypeG], codomain: STypeG) {
  def map(f: STypeG => STypeG): MTypeG =
    MTypeG(typeVar,typeVarBound,domain.map(f), f(codomain))
}
