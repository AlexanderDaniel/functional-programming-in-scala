package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/4.answer.scala
 */
class Exercise4Spec extends Specification {

  "drop" should {
    {
      drop(List(1,2), 0) === List(1,2)
    }.eg

    {
      drop(List(1,2), 1) === List(2)
    }.eg

    {
      drop(List(1,2,3,4), 3) === List(4)
    }.eg

    {
      drop(List(1,2,3,4), 4) === Nil
    }.eg
    
    {
      drop(Nil, 0) === Nil
    }.eg
    
    {
      drop(Nil, 1) should throwA[NoSuchElementException]
    }.eg

    {
      drop(List(1), 2) should throwA[NoSuchElementException]
    }.eg
  }
}