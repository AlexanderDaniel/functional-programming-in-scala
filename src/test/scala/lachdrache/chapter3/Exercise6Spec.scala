package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/6.answer.scala
 */
class Exercise6Spec extends Specification {

  "init" should {
    
    {
      init(List(1,2,3)) === List(1,2)
    }.eg

    {
      init(List(13)) === Nil
    }.eg
    
    {
      init(Nil) must throwA[UnsupportedOperationException]
    }.eg
  }
}