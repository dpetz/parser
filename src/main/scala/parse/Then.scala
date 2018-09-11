package io.parse
import io.Reader

  /** p1 followed by p2 **/
  case class Then[A](p1: Parser[Seq[A]], p2: Parser[Seq[A]]) extends Parser[Seq[A]] {
    def apply(r: Reader) =
      p1(r) { m1 =>
        p2(m1.follow) { m2 => 
        	m1.add(m2){ _ ++: _ } 
        }
      }
  }
