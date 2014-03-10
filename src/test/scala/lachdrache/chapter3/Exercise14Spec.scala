package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/14.answer.scala answer]]
 */
class Exercise14Spec extends Specification {

  "append" should {
    {
      append(Nil, Nil) === Nil
    }.eg

    {
      append(List(1), List(2)) === List(1,2)
    }.eg

    {
      append(List(1,2,3), List(4,5)) === List(1,2,3,4,5)
    }.eg

    {
      append(List(1), Nil) === List(1)
    }.eg

    {
      append(Nil, List(13)) === List(13)
    }.eg

  }
}