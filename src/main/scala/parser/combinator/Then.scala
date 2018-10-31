package parser.combinator
import parser.util.Reader
import parser.{Parser,Result,Match,Fail}

  /** Parses p1 followed by p2 and then appends results of p2 to p1. **/
  case class Then[A](p1: Parser[Seq[A]], p2: Parser[Seq[A]]) extends Parser[Seq[A]] {
    def apply(r: Reader):Result[Seq[A]] =
      p1(r) { m1 =>
        p2(m1.follow) { m2 => 
        	m1.add(m2){ _ ++: _ } 
        }
      }
  }
