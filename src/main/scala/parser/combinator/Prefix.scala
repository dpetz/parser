package parser.combinator

import parser.util.Reader
import parser.{Match, Parser, Result}


/** Consumes [[pre]] and parses [[p]] */
case class Prefix[A, B](pre: Parser[A], p: Parser[B]) extends Parser[B] {
  def apply(r: Reader): Result[B] = pre(r) {
    m: Match[A] =>
      p(
        m.follow)
  }
}