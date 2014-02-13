package lachdrache.chapter2

import org.specs2.mutable.Specification

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/3.answer.scala
 */
class Exercise3Spec extends Specification {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a, b)

  /** using curried of Function2 trait */
  object UsingCurried {
    def curry[A,B,C](f: (A,B) => C): A => (B => C) =
      f.curried

  }

  "curry" should {
    "be spicy" in {
      def f(a: Int, b: Int): Int = a+b
      val f1: (Int) => (Int) => Int = curry(f)
      val f2: (Int) => Int = f1(3)
      f2(10) === 13
    }
  }

  "using curried of Function2 trait" should {
    "be mild" in {
      def f(a: Int, b: Int): Int = a+b
      val f1: (Int) => (Int) => Int = UsingCurried.curry(f)
      val f2: (Int) => Int = f1(3)
      f2(10) === 13
    }
  }

}