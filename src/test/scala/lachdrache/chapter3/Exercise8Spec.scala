package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * What does that say about the relationship between foldRight and the
 * data constructors of List?
 *
 * They build the same nested structure, i.e. the same recursion.
 * - `Cons(1, Cons(2, Cons(3, Nil)))`
 * - `func(1, func(2, func(3, Nil)))`
 *
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/8.hint.txt Hint]] and
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/8.answer.scala answer]]
 */
class Exercise8Spec extends Specification {

  "foldRight" should {
    "construct a list when using Nil as z and Cons as f" in {
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3)
    }
  }
}