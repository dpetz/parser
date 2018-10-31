package parser.combinator
import parser.util.Reader
import parser.{Parser,Result,Match,Fail}

  case class Gate[A,B,C](
  	start:Parser[A],p:Parser[B],end:Parser[C]) extends Parser[B] {

    def apply(r:Reader):Result[B]=Prefix(start,Postfix(p,end))(r)
  }
