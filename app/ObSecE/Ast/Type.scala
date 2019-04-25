package ObSecE.Ast

import Common.{AstNode, NoAstNode, PrettyPrint}

trait STypeE extends EObSecElement with PrettyPrint{
  def privateType: TypeE
  def publicType: LabelE
  def prettyPrint():String
  def map(facetMapper: LabelE => LabelE):STypeE

  def children: List[LabelE]
}
/**
  * Represents a security type
  *
  * @param privateType The private facet
  * @param publicType  The public facet
  */
case class FTypeE(privateType: TypeE, publicType: LabelE) extends STypeE {
  def map(f: TypeE => TypeE, g: LabelE => LabelE): STypeE =
    FTypeE(f(privateType), g(publicType)).setAstNode(this.astNode)

  def map(g: LabelE => LabelE): STypeE =
    FTypeE(g(privateType).asInstanceOf[TypeE], g(publicType)).setAstNode(this.astNode)


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
  override def prettyPrint(): String = {
    val buffer = new StringBuilder
    privateType.prettyPrint(buffer)
    buffer.append("<")
    publicType.prettyPrint(buffer)
    buffer.toString()
  }

  override def children: List[LabelE] = List(privateType,publicType)
}

case class ESTypeE(privateType: TypeE, implType:List[TypeE], publicType: ExistentialFacet)
  extends STypeE {

  /* override def toString: String ={
     val pString: String =
       if(publicType.equals(ParametricObjectType.top) && privateType.equals(ParametricObjectType.top))"H"
       else if(TypeEquivalenceG.alphaEq(publicType,privateType)) "L"
       else s"$publicType"
     s"$privateType<$pString"
   }*/
  override def toString: String = s"SET($privateType,$implType,$publicType)"

  override def prettyPrint(buffer:StringBuilder): Unit = {
    privateType.prettyPrint(buffer)
    buffer.append(" with ")
    if(implType.size>=0) {
      implType.head.prettyPrint(buffer)
      implType.tail.foreach(t => {
        buffer.append(",")
        t.prettyPrint(buffer)
      })
    }
    buffer.append(" as ")
    publicType.prettyPrint(buffer)

  }
  override def prettyPrint(): String = {
    val buffer = new StringBuilder
    prettyPrint(buffer)
    buffer.toString()
  }

  override def map(facetMapper: LabelE => LabelE): ESTypeE =
    ESTypeE(
      facetMapper(privateType).asInstanceOf[TypeE],
      implType.map(x=> facetMapper(x).asInstanceOf[TypeE]),
      facetMapper(publicType).asInstanceOf[ExistentialFacet])

  override def children: List[LabelE] = privateType :: publicType :: implType
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
  * T ::= ... X | X*...
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
trait IObject{
  def methSig(x: String): MTypeE
  def containsMethod(x: String): Boolean
}




//It could be:
// 1. an existential type or
// 2. a type identifier pointing to an existential type
trait ExistentialFacet extends  LabelE



case class ExistentialType(typeVars: List[EVarDecl],methods: List[MethodDeclarationE]) extends ExistentialFacet {
  override def prettyPrint(buffer: StringBuilder): Unit = {
    buffer.append("exists ")
    if(typeVars.size>=0){
      val first =  typeVars.head
      first.prettyPrint(buffer)
      typeVars.tail.foreach(tv=>  {
          buffer.append(", ")
          tv.prettyPrint(buffer)
      })
    }
    buffer.append(" . ")
    buffer.append("[")
    if(methods.size>=0){
      val first = methods.head
      first.prettyPrint(buffer)

      methods.tail.foreach(meth=>  {
        buffer.append(", ")
        meth.prettyPrint(buffer)
      })
    }
    buffer.append("]")
  }
}
case class EVarDecl(name:String,lowerBound:TypeE) extends  PrettyPrint {
  var isAster:Boolean = false
  def setAster(b:Boolean): this.type = {
    isAster = b
    this
  }

  override def prettyPrint(buffer: StringBuilder): Unit = {
    buffer.append(name + " super ")
    lowerBound.prettyPrint(buffer)
  }
}

//it references to a type alias
case class TypeId(name:String) extends TypeE with ExistentialFacet {
  override def prettyPrint(buffer: StringBuilder): Unit = buffer.append(name)
}

case class ObjectType(selfVar: String, methods: List[MethodDeclarationE]) extends TypeE with IObject{


  override def methSig(x: String): MTypeE = methods.find(m=>m.name == x).get.mType

  override def containsMethod(m: String): Boolean = methods.exists(x => x.name == m)

  override def toString: String = s"OT($selfVar,$methods)"

  override def prettyPrint(builder: StringBuilder): Unit =
    if(methods.isEmpty) builder.append("Top")
    else {
      builder.append(s"Obj($selfVar)")
      builder.append("[")
      methods.foreach(e=> {
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
  * Represents a (self) type variable
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

  def st : STypeE = FTypeE(this,this)

  override def toString: String = name
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
    buffer.append(" : ")
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
    buffer.append(" -> ")
    codomain.prettyPrint(buffer)
  }

  var isPrimitive :Boolean = false
  def setPrimitive(b:Boolean):MTypeE = {
    isPrimitive = b
    this
  }
}

trait Primitable{
  var isPrimitive = false
  def setIsPrimitive(b:Boolean):this.type ={
    isPrimitive = b
    this
  }
}

//Non-Surface type
case class RecordTypeE(methods: List[MethodDeclarationE]) extends TypeE with Primitable{
  override def toString: String =  s"{${methods.map(x => x.toString).fold("")((x: String, y: String) => x + y).toString}}"

  override def prettyPrint(builder: StringBuilder): Unit = {
    builder.append("[")
    methods.foreach(e=> {
      builder.append("\n")
      e.prettyPrint(builder)
    })
    builder.append("]")
  }
}