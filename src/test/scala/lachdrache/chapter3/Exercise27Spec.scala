package lachdrache.chapter3

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/27.answer.scala answer]] */
class Exercise27Spec extends Specification {

  "depth" should {
    {
      Tree.depth(Leaf('a')) === 0
    }.eg

    {
      Tree.depth(Branch(Leaf('a'), Leaf('b'))) === 1
    }.eg

    {
      Tree.depth(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))) === 2
    }.eg
  }
}