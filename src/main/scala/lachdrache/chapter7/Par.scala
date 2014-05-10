package lachdrache.chapter7

trait Par[A]

object Par {

  def sumDivideAndConquer(ints: IndexedSeq[Int]): Int =
    if (ints.length<=1)
      ints.headOption getOrElse 0
    else {
      val (left, right) = ints.splitAt(ints.length/2)
      sumDivideAndConquer(left) + sumDivideAndConquer(right)
    }

  def unit[A](a: => A): Par[A] = ???
  def get[A](a: Par[A]): A = ???

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

  object SumReturningPar {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length<=1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length/2)
        val sumL: Par[Int] = sum(l)
        val sumR: Par[Int] = sum(r)
        Par.map2(sumL, sumR)(_ + _)
      }
  }

  /** Note: now we can inline the calls to sum */
  object SumReturningParWithInlinedSums {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length<=1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length/2)
        Par.map2(sum(l), sum(r))(_ + _)
      }

    def evaluating(name: => Unit)(thunk: => Unit): Unit = {
      ()
    }

    object ApplyingTheSubstitutionModel {
      sum(IndexedSeq(1,2,3,4))
      Par.map2(sum(IndexedSeq(1,2)), sum(IndexedSeq(3,4)))
      evaluating(sum(IndexedSeq(1,2))) {
        Par.map2(sum(IndexedSeq(1)), sum(IndexedSeq(2)))
        Par.map2(Par.unit(1), Par.unit(2))
      }
      evaluating(sum(IndexedSeq(3,4))) {
        // ...
      }
    }

  }

  /** Exercise 1
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.hint.txt hint]]
    * and
    * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/1.answer.scala answer]]
    */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    ???

  def fork[A](a: => Par[A]): Par[A] = ???

  object SumWithFork {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length<=1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length/2)
        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }

  }
}
