package lachdrache.chapter9

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // primitives start

  /** Recognize and return a single String*/
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  /** Return the portion of input inspected by p if successful */
  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def attempt[A](p: Parser[A]): Parser[A]

  /** Choose between two parsers, first attempting p1, then o2 if p1 fails */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // primitives end

  /** Always succeed with the value a */
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

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
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a,b)

  def whitespace: Parser[String] = """\s*""".r
  def productIgnoringWhitespace[A,B](pa: Parser[A], pb: Parser[B]): Parser[(A,B)] =
    pa ** whitespace ** pb map { case ((a, _), b) =>
      (a,b)
    }
  /** Apply the function fto the result of p, if successful */
  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
    pa flatMap { a =>
      succeed(f(a))
    }

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    pa flatMap { a =>
      pb map { b =>
        f(a,b)
      }
    }

  def number: Parser[Double] =
    """\d+(.\d*)?""".r map (_.toDouble)

  /** No support of escaping the double quote */
  def stringLiteral: Parser[String] =
    quote ** """[^"]*""".r ** quote map { case ((_, name), _) =>
      name
    }

  def quote: Parser[String] =
    """""""

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
    def ***[B](pb: Parser[B]) = self.productIgnoringWhitespace(p, pb)

    def flatMap[B](f: A=> Parser[B]) = self.flatMap(p)(f)
  }

}

