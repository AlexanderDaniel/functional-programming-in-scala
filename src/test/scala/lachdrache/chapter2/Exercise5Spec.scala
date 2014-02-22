package lachdrache.chapter2

import org.specs2.mutable.Specification

/**
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/5.hint.txt
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/5.answer.scala
 */
class Exercise5Spec extends Specification {

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  "compose" should {
    "return combined function" in {
      def inc(x: Int): Int = x+1
      def tenTimes(x: Int): Int = 10*x
      compose(tenTimes, inc)(1) === 20
    }
  }

  "compose (cheating)" should {
    "return combined function" in {
      val inc = (x: Int) => x+1
      val tenTimes = (x: Int) => 10*x
      (tenTimes compose inc)(1) === 20
    }
  }

  "compose (cheating with andThen)" should {
    "return combined function" in {
      val inc = (x: Int) => x+1
      val tenTimes = (x: Int) => 10*x
      (inc andThen tenTimes)(1) === 20
    }
  }

}