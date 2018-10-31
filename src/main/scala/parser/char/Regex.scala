package parser.char
import parser.util.Reader
import parser.{Parser,Result,Match,Fail}

  /** Find prefix Regex match within next n characters of stream  */
  case class Regex(n: Int)(reStr: String) extends Parser[String] {

    def apply(r: Reader):Result[String] =
      reStr.r.findPrefixMatchOf(r.next(n)) match {
        case Some(m) => Chars(m.end)(r)
        case None => Fail(this, r)
      }

    override def toString = s"Rex($n)($reStr)"
  }
