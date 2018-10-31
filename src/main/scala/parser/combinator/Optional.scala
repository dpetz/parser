package parser.combinator

import parser.util.Reader
import parser.{Parser, Result}


/** Repeats parsing at least `min` an at most `max`. */
case class Optional[A](p: Parser[Seq[A]]) extends Parser[Seq[A]] {
  def apply(r: Reader): Result[Seq[A]] = Repeat(p, 0, 1)(r)
}
