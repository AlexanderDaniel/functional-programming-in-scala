package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/2.answer.scala
 */
class Exercise2Spec extends Specification {

  "tail" should {

    {
      tail(List(1,2,3)) === List(2,3)
    }.eg

    {
      tail(Nil) must throwA[NoSuchElementException]
    }.eg

  }
}