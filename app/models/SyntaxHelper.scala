package models

class SyntaxHelper (val syntaxPath:String) {
  def syntax(): List[SyntaxModel] = List()
}
case class SyntaxModel(productions: List[SyntaxProduction])

case class SyntaxProduction(name:String,items:List[SyntaxItem],category:String)
case class SyntaxItem(name:String,kind:String,description:String)
