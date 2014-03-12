package lachdrache.chapter3

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/25.answer.scala answer]] */
class Exercise25Spec extends Specification {

  "size" should {
    {
      Tree.size(Leaf('a')) === 1
    }.eg

    {
      Tree.size(Branch(Leaf('a'), Leaf('b'))) === 3
    }.eg

    {
      Tree.size(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))) === 5
    }.eg
  }
}