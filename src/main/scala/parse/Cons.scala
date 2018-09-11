package io.parse
import io.Reader

  /** Parses predefined string */
  case class Cons(val s: String) extends Parser[String] {
    def apply(r: Reader) =
      Chars(s.length)(r) { m =>
        if (m.result == s) m else Fail(this, r)
      }

    override def toString = s"Cons($s)"
  }
