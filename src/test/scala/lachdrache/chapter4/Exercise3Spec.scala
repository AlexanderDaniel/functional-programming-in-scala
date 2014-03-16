package lachdrache.chapter4

import org.specs2.mutable.Specification
import lachdrache.chapter4.Option._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/3.hint.txt hint]]
  * and
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/3.answer.scala answer]]
  */
class Exercise3Spec extends Specification {

  "map2" should {
    val add = (a: Int, b: Int) => a+b

    {
      map2(Some(1), Some(2))(add) === Some(3)
    }.eg

    {
      map2(None, Some(2))(add) === None
    }.eg

    {
      map2(Some(1), None)(add) === None
    }.eg

    {
      map2(None, None)(add) === None
    }.eg
  }
}