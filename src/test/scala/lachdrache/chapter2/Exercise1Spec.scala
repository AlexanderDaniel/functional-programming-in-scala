package lachdrache.chapter2

import org.specs2.mutable.Specification

/**
 * Notes
 * - The solution counts down while my solution counts up.
 * - The solution uses less code (none of these ifs).
 */
class Exercise1Spec extends Specification {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go (i: Int, prev2: Int, prev1: Int): Int = {
      if (i>n) prev1
      else go(i+1, prev1, prev2+prev1)
    }

    if (n<1) 0
    else if (n<2) 1
    else go(2, 0, 1)
  }

  /**
   * https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/1.answer.scala
   */
  object Solution {
    def fib(n: Int): Int = {
      @annotation.tailrec
      def loop(n: Int, prev: Int, cur: Int): Int =
        if (n == 0) prev
        else loop(n - 1, cur, prev + cur)
      loop(n, 0, 1)
    }
  }

  testFib("fib", fib)
  testFib("solution", Solution.fib)

  def testFib(h: String, fib: Int => Int) {
    h should {

      {
        fib(0) === 0
      }.eg

      {
        fib(1) === 1
      }.eg

      {
        fib(2) === 1
      }.eg

      {
        fib(3) === 2
      }.eg

      {
        fib(4) === 3
      }.eg

      {
        fib(5) === 5
      }.eg

    }
  }

}