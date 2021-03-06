/**
  * Json parsers and representations.
  *
  * @todo Complete doc
  * @see [[http://www.json.org]]
  * @see [[https://docs.scala-lang.org/style/scaladoc.html#general-style]]
  */
package parser

import parser.Parser.Spaces
import parser.char._
import parser.combinator._
import parser.util.{Reader, Tree}

import scala.util.{Failure, Success, Try}


/** JSON element. Either [[Lit]] or [[Collection]]. */
sealed trait Json {


  case class JsonException(msg: String, json: Json) extends Exception {
    override def toString: String = msg + ": " + json
  }

  private def fail(msg: String) = Failure(JsonException(msg, this))

  def tree: Tree[Json] = Tree(this) {
    case c: Collection => c.values
    case _ => Nil
  }

  def toNum: Try[Num] = fail {
    "Not a number"
  }

  def toArr: Try[Arr] = fail {
    "Not an array"
  }

}


sealed trait Collection extends Json {
  def values: Seq[Json]
}

/** JSON Object Maintains entry order. */
case class Obj(pairs: Map[String, Json]) extends Collection {
  def values: Seq[Json] = pairs.values.toSeq

  override def toString: String = pairs.iterator.map {
    case (k, v) => s""""$k":$v"""
  }.mkString("{", ",", "}")
}

/** JSON Array */
case class Arr(values: Seq[Json]) extends Collection {
  override def toString: String = values.mkString("[", ",", "]")

  override def toArr = Success(this)
}

/** JSON literal, i.e. [[Str]] or [[Num]]. */
sealed trait Lit[A] extends Json {
  def value: A
}

/** JSON number literal. */
case class Num(value: BigDecimal) extends Lit[BigDecimal] {
  override def toString: String = value.toString

  override def toNum = Success(this)
}

/** Provides constructor from [[String]] */
object Num {
  def apply(s: String): Num = Num(BigDecimal(s))
}

/** JSON string literal */
case class Str(value: String) extends Lit[String] {
  override def toString = s""""$value""""
}

object Str {
  /** Constructs from sequence of [[String]]s */
  def apply(cs: Seq[String]) = new Str(cs.mkString(""))
}

/** JSON symbol: [[True]], [[False]], or [[Null]]. */
sealed class Sym(override val toString: String) extends Json

/** JSON `true` singleton. */
case object True extends Sym("true")

/** JSON `null` singleton. */
case object Null extends Sym("null")

/** JSON `false` singleton. */
case object False extends Sym("false")

object Json {

  /** Parses [[Json]] from string or exception if not a valid Json. */
  def parse(parseMe: String): Try[Json] =
    Parsers.JsonParser()(parseMe) match {
      case m: Match[Json] => Success(m.result)
      case f: Fail[Json] => Failure(ParseException(f))
    }

  /** Shortcut for [[parse(String).get]] for strings known to be well-formed. */
  def apply(parseMe: String): Json = parse(parseMe).get

  object Parsers {

    /** Parses number values */
    val num: Parser[Num] = Regex(25)("""[\d-+.eE]+""") > {
      Num(_)
    }
    // TODO support all of https://tools.ietf.org/html/rfc7159#section-6

    /** Parses string values.
      *
      * @todo Escapes, see http://www.json.org */
    val str: Parser[Str] = Gate("\"", Repeat(Regex(1)("[^\"]")), "\"") > {
      Str(_)
    }

    /** Parses "null" string into [[Null]] object. */
    val nul: Parser[Sym] = Cons("null") > { _ => Null }

    /** Parses "true" string into [[True]] object */
    val tru: Parser[Sym] = Cons("true") > { _ => True }

    /** Parses "false" string into [[False]] object */
    val fal: Parser[Sym] = Cons("false") > { _ => False }

    /** Trims whitespaces if any before parsing
      *
      * @see [[Parser.Spaces]]
      */
    def spaces[A](p: Parser[A]): Parser[A] = Gate(Spaces, p, Spaces)

    val arr: Parser[Arr] = Gate("[", Optional(Then(JsonParser(), Repeat(Prefix(",", JsonParser())))), "]") > {
      Arr(_)
    }


    case class JsonParser() extends Parser[Json] {
      def apply(r: Reader): Result[Json] = spaces(num | str | nul | tru | fal | arr | obj)(r)
    }

    val pair: Parser[(String, Json)] = Then(Postfix(spaces(str), ":"), JsonParser()) > {
      seq: Seq[Json] =>
        (
          seq.head.asInstanceOf[Str].value,
          seq(1)
        )
    }

    val obj: Parser[Obj] =
      Gate("{", Then(Optional(pair), Repeat(Prefix(",", pair))), "}") > {
        m: Seq[(String, Json)] => Obj(m.toMap)
      }

  }

}

