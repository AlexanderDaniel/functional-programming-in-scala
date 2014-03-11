package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/20.hint.txt hint]] and
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/20.answer.scala answer]]
 */
class Exercise20Spec extends Specification {

  "flatMap" should {
    {
      flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3)
    }.eg

    {
      flatMap(List(1,2,3))(i => oneTo(i)) === List(1,1,2,1,2,3)
    }.eg

    {
      flatMap(Nil)(i => oneTo(i)) === Nil
    }.eg

    {
      flatMap(List(1,2,3))(i => List(oneTo(i))) === List(List(1), List(1,2), List(1,2,3))
    }.eg

  }
}