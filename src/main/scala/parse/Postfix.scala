package io.parse
import io.Reader

/** Parses [[p]] and consumes [[post]]. */
case class Postfix[A,B](p:Parser[A], post:Parser[B]) extends Parser[A] {
	def apply(r:Reader)= p(r) { m1 => post(m1.follow) {
		m2 => m1.add(m2) { (r1,_) => r1 }
	}}
}