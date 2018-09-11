package io.parse
import io.Reader

  /** Any character of given set */
  case class OneOf(c: Set[Char]) extends Parser[Char] {
    def apply(r: Reader) =
      if (r.hasNext && c(r.char)) Match(r.char, r.next)
      else Fail(this, r)
  }
