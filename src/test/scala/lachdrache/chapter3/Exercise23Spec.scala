package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/23.answer.scala answer]]
 * is even more flexible. The two lists do not have to be of the same type.
 */
class Exercise23Spec extends Specification {

  "zipWith" should {
    {
      zipWith(List(1,2,3), List(4,5,6))(_+_) === List(5,7,9)
    }.eg

    {
      zipWith(List("a", "b", "c"), List("1","2","3"))(_+_) === List("a1", "b2", "c3")
    }.eg

  }
}