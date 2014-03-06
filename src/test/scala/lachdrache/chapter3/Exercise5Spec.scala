package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/5.answer.scala
 */
class Exercise5Spec extends Specification {

  "dropWhile" should {

    {
      dropWhile(List(1,2,3), (x: Int) => x<3) === List(3)
    }.eg

    {
      dropWhile(Nil, (_: Int) => true) === Nil
    }.eg

    {
      dropWhile(List(1,2,3), (_: Int) => true) === Nil
    }.eg

    {
      dropWhile(List(1,2), (x: Int) => x>3) === List(1,2)
    }.eg
  }

}