package ObSec.Parsing

import scala.util.parsing.combinator.PackratParsers

/**
  * Created by racruz on 29-03-2017.
  */
trait DebugPackratParsers extends PackratParsers {
  class Wrap[+T](name:String,parser:Parser[T]) extends PackratParser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val t = parser.apply(in)
      println(name+".apply for token "+first+
        " at position "+pos+ " returns "+t)
      t
    }
  }
}