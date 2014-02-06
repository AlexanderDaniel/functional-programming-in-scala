package lachdrache.chapter2

import org.specs2.mutable.Specification

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

  "fib" should {

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