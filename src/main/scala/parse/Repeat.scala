package io.parse
import io.Reader
import scala.annotation.tailrec

/** Repeats parsing at least `min` an at most `max`. */
  case class Repeat[A](p: Parser[Seq[A]], min: Int = 0, max: Int = Int.MaxValue) extends Parser[Seq[A]] {

    def apply(r: Reader) = {

      @tailrec
      def recurse(
        p: Parser[Seq[A]], r:Reader, min: Int, max: Int,
        ms: Match[Seq[A]]) : Result[Seq[A]]= {

        if (max < 1) return ms
        p(r) match {
          case m: Match[Seq[A]] =>
            recurse( p, m.follow, min - 1, max - 1, ms.add(m){ _ ++: _ } )
          case f: Fail[Seq[A]] =>
            if (min < 1) ms else Fail[Seq[A]](f)
        }

      }
      recurse(p, r, min, max, Match(Vector[A](), r))
    }

  }