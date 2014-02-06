package lachdrache.chapter1

import org.specs2.mutable._

class Chapter1Spec extends Specification {

  /** I was aware of the zip method but unzip was new to me even though it is obvious */
  "unzip" should {
    "split a list of pair into a pair of lists" in {
      val listOfPairs: List[(Int, String)] = List((0,"0"), (1, "1"), (2, "10"), (3, "11"))
      listOfPairs.unzip === (
        List(0,1,2,3),
        List("0", "1", "10", "11")
      )
    }
  }

  /**
   * Meaning of reduce: make smaller or less in amount, degree
   */
  "reduce" should {
    "calculate the sum of numbers" in {
      (1 to 10).reduce {(n1, n2) => n1+n2} === 55
    }
    "just return the one number for a list of size one" in {
      List(13).reduce {(n1, n2) => n1+n2} === 13
    }
    "throws exception for empty list" in {
      List[Int]().reduce {(n1, n2) => n1+n2} should throwA[UnsupportedOperationException]
    }
  }
}
