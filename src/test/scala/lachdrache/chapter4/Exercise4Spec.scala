package lachdrache.chapter4

import org.specs2.mutable.Specification
import lachdrache.chapter4.Option._

/** First I implemented [[sequenceWithPatternMatching]] and then realized that the
  * pattern match is exactly the same as in [[map2]].
  *
  * Intentionally I used `foldRight` even though we miss short-circuiting to train
  * the composition of existing functions.
  *
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/4.hint.txt hint]]
  * suggests that you can also pattern-match on the list.
  *
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/4.answer.scala answer]]
  */
class Exercise4Spec extends Specification {

  assert("sequence with pattern matching", sequenceWithPatternMatching)
  assert("sequence using map2", sequence)
  assert("sequence using recursion", sequenceWithShortCircuit)
  assert("sequence with tail recursion", sequenceTailrec)

  def assert(name: String, sequence: List[Option[Int]] => Option[List[Int]]) {

    name should {
      {
        sequence(List(None)) === None
      }.eg

      {
        sequence(List(Some(1), None)) === None
      }.eg

      {
        sequence(List(Some(1), Some(2))) === Some(List(1,2))
      }.eg

    }
  }
}