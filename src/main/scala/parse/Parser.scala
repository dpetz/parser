package io.parse
import io.Reader

/** Produces a [[Result]] from a [[Reader]]. 
 * Subclasses must implement [[appply(Reader)]]. 
 * @see [[https://en.wikipedia.org/wiki/Parser_combinator]]
 * @see [[https://en.wikipedia.org/wiki/Left_recursion]]
 */
trait Parser[+A] {
  
  /* Parse from character stream. */
  def apply(r: Reader): Result[A]

  /* Shortcut for apply(Reader(s)). */
  def apply(s: String): Result[A] = apply(Reader(s))

  /** Infix operator for `Then(this,p)`. */
  def ~[B >: A](p: Parser[B]):Parser[Seq[B]] = Then(this, p)
  
  /** Infix operator for `Or(this,p)`. */  
  def |[B >: A](p: Parser[B]):Parser[B] = Or(this, p)

  /** Infix operator for `Trans(this,p)`. */  
  def >[B](f:A=>B):Parser[B]=Trans(this,f)

}


object Parser {

  val Spaces = Repeat(OneOf(Set('\n','\t',' ','\r')))

  implicit class Str2Cons(s: String) extends Cons(s)

  implicit class Parser2SeqParser[+A](p: Parser[A]) extends Parser[Seq[A]] {
    def apply(r: Reader) = p(r) map { Vector(_) }
  }
  def test {
    val r = Reader("2017-08-26")
    val p = Repeat(Regex(5)("""\d+-?"""))
    println(p(r).toString)
  }
  
}