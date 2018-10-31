package parser.combinator
import parser.util.Reader
import parser.{Parser,Result,Match,Fail}

/** Translates [[Match]] via function. */
  case class Trans[A,B](p:Parser[A],f:A=>B) extends Parser[B] {
    def apply(r:Reader):Result[B] = p(r) map { f(_) }
  }