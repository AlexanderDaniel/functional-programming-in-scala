package lachdrache.chapter2

import org.specs2.mutable.Specification

class Partial1Spec extends Specification {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  object UsingUnderscore {
    def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
      f(a, _)
  }

  object UsingTypeInference {
    def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
      b => f(a, b)
  }


  "partial1" should {

    def f2(a: Int, b: Int) = a + b

    "return a function with one parameter" in {
      val f1: (Int) => Int = partial1(3, f2)
      f1(10) === 13
    }
  }


  "partial1 using underscore notation" should {

    def f2(a: Int, b: Int) = a + b

    "return a function with one parameter" in {
      val f1: (Int) => Int = UsingUnderscore.partial1(3, f2)
      f1(10) === 13
    }
  }

}