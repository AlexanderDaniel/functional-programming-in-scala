package lachdrache.chapter3

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/28.hint.txt hint]]
  * and
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/28.answer.scala answer]]
  */
class Exercise28Spec extends Specification {

  "map" should {
    {
      Tree.map(Leaf(3))(_+10) === Leaf(13)
    }.eg

    {
      Tree.map(Branch(Leaf(1), Leaf(2)))(_+10) === Branch(Leaf(11), Leaf(12))
    }.eg

    {
      Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(x => -x) === Branch(Leaf(-1), Branch(Leaf(-2), Leaf(-3)))
    }.eg

  }
}