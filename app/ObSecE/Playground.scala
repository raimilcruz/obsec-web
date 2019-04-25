
package ObSecE

import ObSecE.Parsing._
import ObSecE.Runtime.EObSecInterpreter
import ObSecE.Static._

object Playground{
  def main(args:Array[String]):Unit= {
    val program = "let{ \n  type AccountStore = exists X super String.[\n\t   {getPassword : String -> String<X}\n\t   {encrypt : String<X -> Int<L}\n\t]\n\ttype AccountStoreImpl = [\n\t   {getPassword : String -> String}\n\t   {encrypt : String -> Int}\n\t]\n\n\tdeftype AuthServer {\n\t\t{login: String String -> Int}\n\t}\n\tdeftype Binder{\n\t\t{bind: AccountStoreImpl with String as AccountStore -> AuthServer<L}\n\t}\n\tval binder = new {\n\t\tz: Binder<L\n\t\t=>\n\t\tdef bind store =\n\t\t\tnew {z : AuthServer<L =>\n                def login guessPass userName =\n\t\t\t\t\tif store.encrypt(store.getPassword(userName)).==(store.encrypt(guessPass))\n\t\t\t\t\tthen 1 else 0\n\t\t\t}\n\t}\n}\nin\n let{\n\tval accountStoreImpl = new {\n\t\tz : AccountStoreImpl<L\n\t\t=>\n\t\tdef getPassword s = \"qwe123\"\n\t\tdef encrypt s = s.length()\n\t}\n }\n in\n\tbinder.bind(accountStoreImpl).login(\"john\",\"qwe123\")"
    EObSecParser(program) match{
      case Right(ast)=>
        val expr = EOBSecIdentifierResolver(ast)
        //try {
          val theType = EObSecTypeChecker(expr)
          print(theType)
          print("result: "+ EObSecInterpreter.run(expr))
         // assert(theType == STypeG(IntType, IntType))
        //}catch{
        //  case e : TypeErrorG => print(e.analysisError)
       // }
    }
  }
}




