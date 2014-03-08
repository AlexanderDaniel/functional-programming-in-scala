package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/10.answer.scala answer]]
 */
class Exercise10Spec extends Specification {

  "oneTo (constructing a List from 1 to N)" should {
    {
      oneTo(1) === List(1)
    }.eg

    {
      oneTo(13) === List(1,2,3,4,5,6,7,8,9,10,11,12,13)
    }.eg
  }

  "length which uses foldRight" should {
    "create stack overflow for a big list" in {
      List.length(oneTo(10000)) should throwA[StackOverflowError]
    }
  }

  "foldLeft" should {
    "be tail recursive (i.e. no stack overflow)" in {
      def lengthLeft[A](l: List[A]) = foldLeft(l,0)((z,_) => z+1)
      lengthLeft(oneTo(10000)) === 10000
    }
  }


}