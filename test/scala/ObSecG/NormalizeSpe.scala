package scala.ObSecG

import ObSecG.Ast._
import ObSecG.Parsing.{ObSecGIdentifierResolver, ObSecGParser}
import ObSecG.Static._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by racruz on 24-08-2017.
  */
/*
class NormalizeSpe extends FlatSpec with Matchers with ElementServiceBaseSpec{

  "Union type with direct aster label in left position" should "work" in{
    val label = UnionLabel(IntType,LabelVar("l").setAster(true))

    val auxiliaryFunctions = new AuxiliaryFunctions
    val result = auxiliaryFunctions.normalize(label,BoundedLabelVar("l",IntType,ObjectType.top))

    assert(result == IntType)
  }
  "Union type with direct aster label in right position" should "work" in{
    val label = UnionLabel(LabelVar("l").setAster(true),IntType)

    val auxiliaryFunctions = new AuxiliaryFunctions
    val result = auxiliaryFunctions.normalize(label,BoundedLabelVar("l",IntType,ObjectType.top))

    assert(result == IntType)
  }
  "Union type with indirect aster label in left position" should "work" in{
    val label = UnionLabel(UnionLabel(LabelVar("l").setAster(true),StringType),IntType)

    val auxiliaryFunctions = new AuxiliaryFunctions
    val result = auxiliaryFunctions.normalize(label,BoundedLabelVar("l",IntType,ObjectType.top))

    assert(result ==  UnionLabel(StringType,IntType))
  }
  "Union type with indirect aster label in right position" should "work" in{
    val label = UnionLabel(IntType,UnionLabel(LabelVar("l").setAster(true),StringType))

    val auxiliaryFunctions = new AuxiliaryFunctions
    val result = auxiliaryFunctions.normalize(label,BoundedLabelVar("l",IntType,ObjectType.top))

    assert(result == UnionLabel(IntType,StringType))
  }
  "Union type with indirect 2 aster label in right position" should "work" in{
    val label = UnionLabel(IntType,UnionLabel(UnionLabel(LabelVar("l").setAster(true),LabelVar("l").setAster(true)),StringType))

    val auxiliaryFunctions = new AuxiliaryFunctions
    val result = auxiliaryFunctions.normalize(label,BoundedLabelVar("l",IntType,ObjectType.top))

    assert(result == UnionLabel(IntType,StringType))
  }
}
*/
