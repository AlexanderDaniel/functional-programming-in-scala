package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/21.answer.scala answer]]
 */
class Exercise21Spec extends Specification {

  "filter via flatMap" should {
    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l) { a =>
        if (p(a)) List(a)
        else Nil
      }

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