package lachdrache.chapter3

import org.specs2.mutable.Specification

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/9.answer.scala answer]]
 */
class Exercise9Spec extends Specification {

  "length" should {
    {
      List.length(Nil) === 0
    }.eg

    {
      List.length(List('a')) === 1
    }.eg

    {
      List.length(List(1,2,3,4,5)) === 5
    }.eg
  }
}