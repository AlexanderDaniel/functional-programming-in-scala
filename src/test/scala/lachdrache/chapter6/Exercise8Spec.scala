package lachdrache.chapter6

import org.specs2.mutable.Specification
import lachdrache.chapter6.RNG._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/8.hint.txt hint]]
  * and [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/10.answer.scala answer]]
  */
class Exercise8Spec extends Specification {

  object ConstRng extends RNG {
    override def nextInt: (Int, RNG) = (13, this)
  }

  "flatMap" should {
    {
      flatMap(unit(1))(a => unit(a+1))(ConstRng) === unit(2)(ConstRng)
    }.eg
  }

}