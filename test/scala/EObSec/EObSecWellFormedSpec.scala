package scala.EObSec

import Common._
import ObSecE.Ast._
import ObSecE.Parsing.{EOBSecIdentifierResolver, EObSecParser, TypeIdentifierDeclarationPoint}
import ObSecE.Static._
import org.scalatest.{FlatSpec, Matchers}


class EObSecWellFormedSpec extends FlatSpec with Matchers with BaseSpec {

  "Type alias " must "be properly handled" in {
    //arrange
    val errorCollector = new ErrorCollector
    val judgments = new EObSecGJudgmentImpl(errorCollector)

    val resolvedType = FTypeE(TypeId("A"),TypeId("A"))

    val typeAliasScope = new NestedScope[LabelE](null)
    typeAliasScope.add("A",ObjectType("x",List()))

    val existentialVariableEnvironment = Environment.empty[TypeE]()
    //act
    val isWellFormed = judgments.isWellFormed(typeAliasScope,existentialVariableEnvironment,resolvedType)
    //assert
    assert(isWellFormed)
  }
  "Type alias 2 " must "be properly handled" in {
    //arrange
    val errorCollector = new ErrorCollector
    val judgments = new EObSecGJudgmentImpl(errorCollector)

    val resolvedType = FTypeE(TypeId("A"),TypeId("B"))

    val typeAliasScope = new NestedScope[LabelE](null)
    typeAliasScope.add("A",ObjectType("x",List()))
    typeAliasScope.add("B",ObjectType("x",List()))

    val existentialVariableEnvironment = Environment.empty[TypeE]()
    //act
    val isWellFormed = judgments.isWellFormed(typeAliasScope,existentialVariableEnvironment,resolvedType)
    //assert
    assert(isWellFormed)
  }
  "Type alias 3 " must "be properly handled" in {
    //arrange
    val errorCollector = new ErrorCollector
    val judgments = new EObSecGJudgmentImpl(errorCollector)

    val resolvedType = FTypeE(TypeId("A"),
                        ObjectType("x",
                            List(MethodDeclarationE("m",MTypeE(List(),FTypeE(IntADT,IntADT)))))
                          )

    val typeAliasScope = new NestedScope[LabelE](null)
    typeAliasScope.add("A",ObjectType("x",List()))

    val existentialVariableEnvironment = Environment.empty[TypeE]()
    //act
    try{
      judgments.isWellFormed(typeAliasScope, existentialVariableEnvironment, resolvedType)
      fail()
    }
    catch{
      case c:CommonError=> assert(c.analysisError.errorCode == CommonErrorCodes.facetedWiseSubtypingError)
      case _ => fail()
    }
  }

}
