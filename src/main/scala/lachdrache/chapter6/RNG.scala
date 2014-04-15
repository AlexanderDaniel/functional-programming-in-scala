package lachdrache.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /** Exercise 1
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/1.answer.scala answer]]
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng1)
  }

  /** Exercise 2
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/2.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/2.answer.scala solution]]
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val d = n.toDouble / (Int.MaxValue.toDouble + 1)
    (d, rng2)
  }

}
