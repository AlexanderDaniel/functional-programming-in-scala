package lachdrache.chapter4

import org.specs2.mutable.Specification
import lachdrache.chapter4.Option._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/5.hint.txt hint]]
  * and
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/5.answer.scala answer]]
  */
class Exercise5Spec extends Specification {

  "traverse" should {
    {
      traverse(List(1,2))(Some(_)) === Some(List(1,2))
    }.eg

    {
      traverse(List(1,2))(x => if (x%2==0) Some(x) else None) === None
    }.eg

  }
}