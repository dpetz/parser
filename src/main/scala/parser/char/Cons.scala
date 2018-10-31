package parser.char
import parser.util.Reader
import parser.{Parser,Result,Match,Fail}

  /** Parses predefined string */
  case class Cons(s: String) extends Parser[String] {
    def apply(r: Reader):Result[String]       =
      Chars(s.length)(r) { m =>
        if (m.result == s) m else Fail(this, r)
      }

    override def toString = s"Cons($s)"
  }
