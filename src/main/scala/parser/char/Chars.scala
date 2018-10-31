package parser.char

import parser.util.Reader
import parser.{Match, Parser}
import scala.collection.mutable.ArrayBuffer

/** Parses next n characters from stream
  * (or until its end) into string. */
case class Chars(n: Int) extends Parser[String] {

  def apply(start: Reader): Match[String] = {
    val buf = new ArrayBuffer[Char](n)

    def consume(r: Reader, i: Int): Reader = {
      if (r.hasNext) {
        buf += r.char
        r.next
      } else r
    }

    val end = (1 to n).foldLeft(start)(consume)
    Match(buf.mkString(""), end)
  }

}