package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/11.answer.scala answer]] */
class Exercise11Spec extends Specification {

  "sumLeft" should {
    {
      sumLeft(Nil) === 0
    }.eg
    
    {
      sumLeft(List(1,2,3)) === 6
    }.eg

    /** http://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_⋯ */
    "return correct value for big numbers" in {
      sumLeft(oneTo(10000)) === (10000/2*10001)
    }
  }

  "productLeft" should {
    {
      productLeft(Nil) === 1.0
    }.eg

    {
      productLeft(List(2.0, 3.0, 4.0)) === 24.0
    }.eg

  }

  "lengthLeft" should {
    {
      lengthLeft(Nil) === 0
    }.eg

    {
      lengthLeft(oneTo(100000)) === 100000
    }.eg
  }
}