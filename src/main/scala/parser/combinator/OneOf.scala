package parser.combinator

import parser.util.Reader
import parser.{Fail, Match, Parser, Result}


/** Any character of given set */
case class OneOf(c: Set[Char]) extends Parser[Char] {
  def apply(r: Reader): Result[Char] =
    if (r.hasNext && c(r.char)) Match(r.char, r.next)
    else Fail(this, r)
}
