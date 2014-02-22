package lachdrache.chapter2

import org.specs2.mutable.Specification

/**
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/4.answer.scala
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/4.hint.txt
 */
class Exercise4Spec extends Specification {

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  object Cheating {
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
      Function.uncurried(f)
  }

  "uncurry" should {
    "return a function with two parameters" in {
      val f: Int => Int => Int = (a: Int) => (b: Int) => a+b
      f(1)(2) === 3
      uncurry(f)(1, 2) === 3
    }
  }

  "uncurry (cheating)" should {
    "return a function with two parameters" in {
      val f: Int => Int => Int = (a: Int) => (b: Int) => a+b
      f(1)(2) === 3
      Cheating.uncurry(f)(1, 2) === 3
    }
  }
}