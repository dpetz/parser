package io.parse
import io.Reader

  case class Gate[A,B,C](
  	start:Parser[A],p:Parser[B],end:Parser[C]) extends Parser[B] {

    def apply(r:Reader)=Prefix(start,Postfix(p,end))(r)
  }
