package io.parse
import io.Reader

  /** Parses [[p1]] if matching otherwise [[p2]] */
  case class Or[A](p1:Parser[A], p2:Parser[A]) extends Parser[A] {
    def apply(r: Reader) = p1(r).atFail { _ => p2(r) }
  }
