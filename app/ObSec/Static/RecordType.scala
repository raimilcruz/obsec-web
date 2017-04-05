package ObSec.Static

import ObSec.Ast.{MType, MethodDeclaration, Type}

/**
  * Created by racruz on 04-04-2017.
  * Internal record type (used when a object recursive type is unfolded
  */
case class RecordType(methods: List[MethodDeclaration]) extends Type {
  override def toString: String =  s"{${methods.map(x=>x.toString).fold("")((x:String,y:String)=> x+y).toString()}}"
  override def methSig(x: String): MType = throw new NotImplementedError("Not important")
  override def containsMethod(x: String): Boolean = throw new NotImplementedError("Not important")
}
