package lachdrache.chapter9

import MyParsersTypes._

import scala.util.matching.Regex
import scala.language.implicitConversions

object MyParsersTypes {
  type Parser[+A] = Location => Result[A]

  trait Result [+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }
    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, false) if isCommitted => Failure(e, true)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean=true) extends Result[Nothing]
}

object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Failure(parseError, _) => Left(parseError)
      case Success(a, _) => Right(a)
    }

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
        case Failure(parseError, committed) => Failure(parseError, committed)
      }
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  /** attempt{p) converts committed failures of p to uncommitted failures */
  override def attempt[A](p: Parser[A]): Parser[A] = ???

  /** Choose between two parsers, first attempting p1, then o2 if p1 fails */
  override def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    loc => x(loc) match {
      case r@Failure(e, committed) if committed => y(loc)
      case r => r
    }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] =
    l => p(l) match {
      case Success(a,n) => f(a)(l.advanceBy(n))
        .addCommit(n == 0)
        .advanceSuccess(n)
      case f@Failure(_,_) => f
    }

}
