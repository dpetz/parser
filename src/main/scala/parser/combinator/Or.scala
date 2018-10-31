package parser.combinator

import parser.util.Reader
import parser.{Parser, Result}

/** Parses [[p1]] if matching otherwise [[p2]] */
case class Or[A](p1: Parser[A], p2: Parser[A]) extends Parser[A] {
  def apply(r: Reader): Result[A] = p1(r).atFail { _ => p2(r) }
}
