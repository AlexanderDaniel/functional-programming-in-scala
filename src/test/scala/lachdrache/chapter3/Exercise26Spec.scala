package lachdrache.chapter3

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/26.answer.scala answer]] */
class Exercise26Spec extends Specification {

  "maximum" should {
    {
      Tree.maximum(Leaf(13)) === 13
    }.eg

    {
      Tree.maximum(Branch(Leaf(13), Leaf(1))) === 13
    }.eg

    {
      Tree.maximum(Branch(Leaf(12), Branch(Leaf(13), Leaf(7)))) === 13
    }.eg
  }
}