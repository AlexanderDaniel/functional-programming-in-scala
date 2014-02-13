package lachdrache.chapter2

import org.specs2.mutable.Specification

class Exercise3Spec extends Specification {

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  "curry" should {
    "be spicy" in {
      def f(a: Int, b: Int): Int = a+b
      val f1: (Int) => (Int) => Int = curry(f)
      val f2: (Int) => Int = f1(3)
      f2(10) === 13
    }
  }

}