package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/19.answer.scala answer]]
 */
class Exercise19Spec extends Specification {

  "filter" should {
    {
      filter(List(1,2))(_%2==0) === List(2)
    }.eg

    {
      filter(Nil)(identity) === Nil
    }.eg

    {
      filter(List("Scala", "Pascal", "Python"))(s => s.contains("a")) === List("Scala", "Pascal")
    }.eg
  }
}