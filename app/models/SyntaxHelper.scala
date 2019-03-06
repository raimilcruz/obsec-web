package models

import java.io.File
import java.nio.file.Paths

import play.api.libs.json._

import scala.io.Source

class SyntaxHelper (val syntaxPath:String) {
  def syntax(): SyntaxModel = {
    val fileContents = Source.fromFile(syntaxPath).getLines.mkString
    SyntaxModel(parseJson(fileContents))
  }



  private def parseJson(syntaxJson: String): List[SyntaxProduction] = {
    val json = Json.parse(syntaxJson)
    val jsonObject = json.asInstanceOf[JsObject]
    if(jsonObject !=null){
      return jsonObject.fields
        .map(p=> parseProductionRule(p._1,p._2)).toList
    }
    throw new Error("Syntax Json Format Error: Production rules are specified in a json object")
  }
  private def parseProductionRule(name: String, json: JsValue): SyntaxProduction = {
    val expanded = (json \ "expanded").toOption match {
      case None =>false
      case Some(x)=> true
    }
    if(expanded){
      val expansion : String = (json \ "expansion").get
      return SyntaxProduction(name,expanded,expansion,List(),"")
    }
    val category : String = (json \ "category").get
    json \ "items" match {
      case jsonArray:JsArray=>
        val items =  jsonArray.value.map(parseSyntaxItem)
        SyntaxProduction(name,expanded,"",items.toList,category)
      case jsonDefined:JsDefined =>
        val items =  jsonDefined.value.asInstanceOf[JsArray].value.map(parseSyntaxItem)
        SyntaxProduction(name,expanded,"",items.toList,category)
      case x =>
        throw new Error(s"Syntax Json Format Error: Symbols are specified in an array ${x.toString}")
    }
  }
  private def parseSyntaxItem(jsonSymbol: JsValue):SyntaxItem={
    jsonSymbol match {
      case jsonObject: JsObject =>
        val name:String = (jsonObject \ "item").get
        val expandible = (jsonObject \ "expanded").toOption match {
          case None => false
          case Some(x) => true
        }
        SyntaxItem(name, expandible)
      case jsonString: JsString => SyntaxItem(jsonString.value,expanded = false)
      case _ => throw new Error("Syntax Json Format Error: Symbols are either a string or a json object")
    }
  }
  implicit def jsonValueToString(jsValue:JsValue):String = jsValue match {
    case s:JsString => s.value
    case _ => ""
  }
}
case class SyntaxModel(productions: List[SyntaxProduction])

case class SyntaxProduction(name:String,expanded:Boolean,expansion:String,
                            items:List[SyntaxItem],category:String)
case class SyntaxItem(name:String,expanded:Boolean)
