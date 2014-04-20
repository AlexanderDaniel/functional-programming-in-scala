package lachdrache.chapter6

import org.specs2.mutable.Specification
import lachdrache.chapter6.RNG._

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/7.hint.txt hint]]
  * and
  * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/9.answer.scala answer]]
  */
class Exercise7Spec extends Specification {

  "sequence" should {
    case class ConstRng() extends RNG {
      override def nextInt: (Int, RNG) = (13, this)
    }

    {
      sequenceWithRecursion(List(unit(1), unit(2), unit(3)))(ConstRng()) === unit(List(1,2,3))(ConstRng())
    }.eg

    {
      sequence(List(unit(1), unit(2), unit(3)))(ConstRng()) === unit(List(1,2,3))(ConstRng())
    }.eg
  }
}