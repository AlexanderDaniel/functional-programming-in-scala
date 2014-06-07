package lachdrache.chapter8

import lachdrache.chapter8.Prop._
import lachdrache.chapter6.RNG
import lachdrache.chapter5.Stream

case class Prop(run: (TestCases, RNG) => Result)

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) None else Some((a.toString, i))
        } catch {
          case e: Exception => Some(buildMsg(a, e), i)
        }
      }
      .find(_.isDefined).getOrElse(None)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
