package lachdrache.chapter9

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /** Recognize and return a single String*/
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  /** Always succeed with the value a */
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  /** Return the portion of input inspected by p if successful */
  def slice[A](p: Parser[A]): Parser[String]

  /** Choose between two parsers, first attempting p1, then o2 if p1 fails */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    n match {
      case 0 => succeed(List.empty[A])
      case _ => map2(p, listOfN(n-1, p))(_ :: _)
    }

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List.empty[A])

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p ** many(p) map { case (a0, as) =>
      a0 :: as
    }
  def many1ViaMap2[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  /** Sequence two parsers, running p1, than p2 and return the pair of their results if both succeed */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  /** Apply the function fto the result of p, if successful */
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    (pa ** pb) map f.tupled

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def num(c: Char): Parser[Int] = {
    char(c).many.slice map (_.size)
  }

  val numA: Parser[Int] = num('a')

  val asAndBs: Parser[Int] =
    (num('a') ** num('b')) map { case (a, b) =>
      a + b
    }

  val digitAndAs: Parser[List[Char]] =
    """\d""".r flatMap { n =>
      listOfN(n.toInt, char('a'))
    }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice = self.slice(p)

    def many = self.many(p)

    def many1 = self.many1(p)

    def product[B](p2: Parser[B]) = self.product(p, p2)

    def **[B](p2: Parser[B]) = self.product(p, p2)

    def flatMap[B](f: A=> Parser[B]) = self.flatMap(p)(f)
  }

}

