package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/** https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/12.answer.scala */
class Exercise12Spec extends Specification {

  "reverse" should {
    {
      reverse(Nil) === Nil
    }.eg

    {
      reverse(List(1)) === List(1)
    }.eg

    {
      reverse(List(1,2)) === List(2,1)
    }.eg

    {
      reverse(List(1,2,3,4,5)) === List(5,4,3,2,1)
    }.eg
  }
}