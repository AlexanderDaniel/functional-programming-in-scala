package lachdrache.chapter9

import scala.util.matching.Regex
import scala.language.implicitConversions

trait MyParser[+A] {
  def run(input: String): (Either[ParseError, A], String)
}

object MyParsers extends Parsers[MyParser] {
  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
    p.run(input)._1

  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] =
    new MyParser[B] {
      override def run(input: String) = {
        val (o1, r1) = p.run(input)
        o1.fold(
          parseError => (Left(parseError), input),
          a => f(a).run(r1)
        )
      }
    }

  /** Choose between two parsers, first attempting p1, then o2 if p1 fails */
  override def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = ???

  /** Recognize and return a single String */
  override implicit def string(s: String): MyParser[String] =
    new MyParser[String] {
      override def run(input: String) =
        if (input.startsWith(s)) (Right(s), input.drop(s.length))
        else (Left(ParseError()), input)
    }

  override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  override implicit def regex(r: Regex): MyParser[String] =
    new MyParser[String] {
      override def run(input: String) =
        r.findPrefixOf(input)
          .map(matchedStr => (Right(matchedStr), input.drop(matchedStr.length)))
          .getOrElse((Left(ParseError()), input))
    }

  /** Return the portion of input inspected by p if successful */
  override def slice[A](p: MyParser[A]): MyParser[String] = ???

  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  override def attempt[A](p: MyParser[A]): MyParser[A] = ???
}
