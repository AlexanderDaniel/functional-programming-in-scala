package lachdrache.chapter4

import org.specs2.mutable.Specification

/** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/errorhandling/2.answer.scala answer]] */
class Exercise2Spec extends Specification {

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs map {x => math.pow(x-m, 2)})
    }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

}