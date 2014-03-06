package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

class Exercise1Spec extends Specification {

  "exercise 1" should {

    "return the 1+2 (third match expression)" in {
      val result = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      result === 3
    }
  }
}