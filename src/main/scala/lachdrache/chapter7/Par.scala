package lachdrache.chapter7

trait Par[A]

object Par {

  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???

  def sumDivideAndConquer(ints: IndexedSeq[Int]): Int =
    if (ints.length<=1)
      ints.headOption getOrElse 0
    else {
      val (left, right) = ints.splitAt(ints.length/2)
      sumDivideAndConquer(left) + sumDivideAndConquer(right)
    }

  object SumWithUnitAndGet {
    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.length<=1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length/2)
        val sumL: Par[Int] = unit(sum(l))
        val sumR: Par[Int] = unit(sum(r))
        get(sumL) + get(sumR)
      }
  }

  /**
   * Referential transparency is broken because the program is
   * no longer parallel.
   *
   * `get` is strict in it's argument hence
   * 1. `unit(sum(l))`
   * 2. `get(unit(sum(l)))` blocks until the sum of the left part is computed
   * 3. `get(unit(sum(r)))` since it is an argument to `+`
   * 4. `+`
   */
  object SumWithUnitAndGetButNoLongerParallel {
    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.length<=1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length/2)
        get(unit(sum(l))) + get(unit(sum(r)))
      }
  }


  /** Exercise 1
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.answer.scala answer]]
    */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    ???


}
