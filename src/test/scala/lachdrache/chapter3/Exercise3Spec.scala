package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/3.answer.scala
 */
class Exercise3Spec extends Specification {

  "setHead" should {
    {
      setHead(List(0,2), 1) === List(1,2)
    }.eg
    
    {
      setHead(Nil, 1) should throwA[NoSuchElementException]
    }.eg

  }
}