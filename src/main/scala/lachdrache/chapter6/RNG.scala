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

  /** Exercise 3
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/3.answer.scala answer]]
    */
  def intDoublePair(rng0: RNG): ((Int, Double), RNG) = {
    val (n, rng1) = rng0.nextInt
    val (d, rng2) = double(rng1)
    ((n,d), rng2)
  }

  def doubleIntPair(rng0: RNG): ((Double, Int), RNG) = {
    val (pair, rng1) = intDoublePair(rng0)
    (pair.swap, rng1)
  }

  def double3(rng0: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng0)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  /** Exercise 4
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/state/4.answer.scala answer]]
    */
  def int(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, acc: (List[Int], RNG)): (List[Int], RNG) =
      (n, acc) match {
        case (0, _) => acc
        case (_, (tail, rng0)) =>
          val (n, rng1) = rng0.nextInt
          go(n-1, (n :: tail, rng1))
      }
    go(count, (Nil, rng))
  }
  
}
