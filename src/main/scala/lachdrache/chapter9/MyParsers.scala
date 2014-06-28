package lachdrache.chapter9

import MyParsersTypes._

import scala.util.matching.Regex
import scala.language.implicitConversions

object MyParsersTypes {
  type Parser[+A] = Location => Result[A]

  trait Result [+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] =
    location => {
      val result = p(location)
      result match {
        case Success(a, charsConsumed) => f(a)(location.advanceBy(charsConsumed))
        case Failure(parseError) => Failure(parseError)
      }
    }

  /** Choose between two parsers, first attempting p1, then o2 if p1 fails */
  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  /** Recognize and return a single String */
  override implicit def string(s: String): Parser[String] =
    location => {
      if (location.currentInput.startsWith(s)) Success(s, s.length)
      else Failure(location.toError(s"$s expected"))
    }

  override implicit def regex(r: Regex): Parser[String] =
    location => {
      r.findPrefixOf(location.currentInput)
        .map { s => Success(s, s.length)}
        .getOrElse { Failure(location.toError(s"Regex $r expected"))}
    }

  /** Return the portion of input inspected by p if successful */
  override def slice[A](p: Parser[A]): Parser[String] =
    location => {
      p(location) match {
        case Success(_, charsConsumed) => Success(location.currentInput.take(charsConsumed), 0)
        case Failure(parseError) => Failure(parseError)
      }
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  /** attempt{p) converts committed failures of p to uncommitted failures */
  override def attempt[A](p: Parser[A]): Parser[A] = ???
}
