package io.parse
import io.Reader

/** Consumes [[pre]] and parses [[p]] */
case class Prefix[A,B](pre:Parser[A], p:Parser[B]) extends Parser[B] {
	def apply(r:Reader)= pre(r) {
		m:Match[A] => 
		p(
			m.follow) }
}