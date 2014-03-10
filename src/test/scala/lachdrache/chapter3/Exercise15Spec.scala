package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

class Exercise15Spec extends Specification {

  "flatten" should {
    {
      concat(List(Nil, Nil)) === Nil
    }.eg

    {
      concat(List(List(1,2), List(3))) === List(1,2,3)
    }.eg

    {
      concat(List(List(1), List(2,3), List(4,5,6))) === List(1,2,3,4,5,6)
    }.eg
  }
}