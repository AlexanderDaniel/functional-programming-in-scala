package lachdrache.chapter2

import org.specs2.mutable.Specification

/**
 * Links
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/2.answer.scala
 * - https://github.com/pchiusano/fpinscala/blob/master/answerkey/gettingstarted/2.hint.txt
 */
class Exercise2Spec extends Specification {

  def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i>=as.length) true
      else if (f(as(i-1), as(i))) loop(i+1)
      else false

    loop(1)
  }

  "isSorted for ints" should {
    val lt: (Int, Int) => Boolean = (a,b) => a<=b

    "return true for empty array" in {
      true === isSorted(Array[Int](), lt)
    }

    "return true for one element" in {
      true === isSorted(Array(13), lt)
    }

    "return true for 3, 13" in {
      true === isSorted(Array(3, 13), lt)
    }

    "return false for 13, 3" in {
      false === isSorted(Array(13, 3), lt)
    }

    "return true for 1,1,1,2,2" in {
      true === isSorted(Array(1,1,1,2,2), lt)
    }

    "return true for 1,2,3,4,5,6,7,8,9,10" in {
      true === isSorted(Array(1,2,3,4,5,6,7,8,9,10), lt)
    }

    "return false for 1,2,3,4,5,6,7,8,9,0" in {
      false === isSorted(Array(1,2,3,4,5,6,7,8,9,0), lt)
    }

  }
}