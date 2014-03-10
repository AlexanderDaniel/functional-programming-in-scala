package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/17.answer.scala answer]]
 */
class Exercise17Spec extends Specification {

  "doubleToString" should {
    {
      doubleToString(Nil) === Nil
    }.eg

    {
      doubleToString(List(1.0, 2.0, 3.0)) === List("1.0", "2.0", "3.0")
    }.eg
  }
}