package ObSec.Ast

/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class SType(privateType: Type, publicType: Type) {
  def map(f: Type => Type): SType =
    SType(f(privateType), f(publicType))

  override def toString: String = s"$privateType<$publicType"
}

/**
  * Represents an abstract type in ObSec
  * T ::= Obj(X)[mtype ...] | X
  */
trait Type{
  def methSig(x: String):MType
  def containsMethod(x:String):Boolean
}

/**
  * Represents an object recursive type
  *
  * @param typeVar The type variable
  * @param methods The list of method signatures of the object type
  */
case class ObjType(typeVar: TypeVar, methods: List[MethodDeclaration]) extends Type {
  override def methSig(x: String): MType = {
    methods.find(m=>m.name==x).get.mtype
  }
  override def containsMethod(m: String):Boolean = methods.exists(x => x.name == m)

  override def toString: String =
    s"{ot ${typeVar.name} ${methods.map(x=>x.toString).fold("")((x:String,y:String)=> x+y).toString()}}"
}
object ObjType{
  val top = ObjType(TypeVar("x"),List())
}

/**
  * Represents a type variable
  *
  * @param name The variable name
  */
case class TypeVar(name: String) extends Type{
  override def methSig(x: String): MType = throw new Error("Type var does not have methods")

  override def containsMethod(x: String): Boolean = throw new Error("Type var does not have methods")

  override def toString: String = name
}
trait PrimType{
  def toObjType:ObjType
}
case object IntType extends Type with PrimType {
  override def methSig(x: String): MType = x match {
    case "+" => MType(SType(IntType, IntType), SType(IntType, IntType))
    case "-" => MType(SType(IntType, IntType), SType(IntType, IntType))
    case _ => throw new Error("Message not understood")
  }

  override def containsMethod(x: String): Boolean = x match {
    case "+" | "-" => true
    case _ => false
  }

  override def toString: String = "Int"

  override def toObjType(): ObjType =
    ObjType(TypeVar("x"),
      List(
        MethodDeclaration("add",
          MType(
            SType(TypeVar("x"), TypeVar("x")),
            SType(TypeVar("x"), TypeVar("x")))),
        MethodDeclaration("minus",
          MType(
            SType(TypeVar("x"), TypeVar("x")),
            SType(TypeVar("x"), TypeVar("x"))))))
}

case object StringType extends Type with PrimType {
  override def methSig(x: String): MType = throw new NotImplementedError()

  override def containsMethod(x: String): Boolean = throw new NotImplementedError()

  override def toString: String = "String"

  override def toObjType(): ObjType = ObjType(TypeVar("x"),List())
}
case object BooleanType extends Type with PrimType{
  override def methSig(x: String): MType = throw new NotImplementedError()

  override def containsMethod(x: String): Boolean = throw new NotImplementedError()

  override def toString: String = "Bool"
  override def toObjType(): ObjType = ObjType(TypeVar("x"),List())

}


sealed trait LabelType extends Type{
  override def methSig(x: String): MType = throw new Error("It does not sense")
  override def containsMethod(x: String): Boolean = throw new Error("It does not sense")
}
case object LowLabel extends LabelType
case object HighLabel extends LabelType

/**
  * Represent a method signature
  *
  * @param name  The method label
  * @param mtype The method type
  */
case class MethodDeclaration(name: String, mtype: MType){
  override def toString: String = s"{$name : $mtype }"
}

/**
  * Represents a method type S1 -> S2
  *
  * @param domain The domain type
  * @param codomain The codomain type
  */
case class MType(domain : SType, codomain: SType) {
  def map(f: SType => SType): MType =
    MType(f(domain), f(codomain))

  override def toString: String = s"$domain -> $codomain"
}
