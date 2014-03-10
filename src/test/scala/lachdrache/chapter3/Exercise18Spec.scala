package lachdrache.chapter3

import org.specs2.mutable.Specification

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/18.answer.scala answer]]
 */
class Exercise18Spec extends Specification {

  "map" should {
    {
      List.map(Nil)(identity) === Nil
    }.eg

    {
      List.map(List(1,2,3))(_*10) === List(10, 20, 30)
    }.eg
  }
}