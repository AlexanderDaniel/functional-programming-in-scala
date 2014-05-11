package lachdrache.chapter7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}


object Par {

  type Par[A] = ExecutorService => Future[A]

  def sumDivideAndConquer(ints: IndexedSeq[Int]): Int =
    if (ints.length<=1)
      ints.headOption getOrElse 0
    else {
      val (left, right) = ints.splitAt(ints.length/2)
      sumDivideAndConquer(left) + sumDivideAndConquer(right)
    }

  // Replaced by strict version version and lazyUnit below
  // def unit[A](a: => A): Par[A] = ???
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
      Par.map2(sum(IndexedSeq(1,2)), sum(IndexedSeq(3,4)))(_ + _)
      evaluating(sum(IndexedSeq(1,2))) {
        Par.map2(sum(IndexedSeq(1)), sum(IndexedSeq(2)))(_ + _)
        Par.map2(Par.unit(1), Par.unit(2))(_ + _)
      }
      evaluating(sum(IndexedSeq(3,4))) {
        // ...
      }
    }

  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a) // (1)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // (2)
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // (3)
    }

  def fork[A](a: => Par[A]): Par[A] = // (4)
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortParViaMap2(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((l, _) => l.sorted)
  
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortParViaMap(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(l => l.sorted)

  /** For me it is hard to see why this version with foldRight runs everything in parallel */
  def parMapViaFoldRightAndMap2[A,B](l: List[A])(f: A => B): Par[List[B]] =
    l.foldRight(unit(List.empty[B])) { (a, b) =>
      map2(fork(unit(f(a))), b)((a, b) => a :: b)
    }

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = l match {
    case Nil => unit(Nil)
    case a :: as =>
      val b: Par[B] = fork(unit(f(a)))
      val bs: Par[List[B]] = fork(parMap(as)(f))
      map2(b, bs)(_ :: _)
  }

  def parMapUsingAsyncF[A,B](l: List[A])(f: A => B): Par[List[B]] = l match {
    case Nil => unit(Nil)
    case a :: as =>
      val b: Par[B] = asyncF(f)(a)
      val bs: Par[List[B]] = fork(parMap(as)(f))
      map2(b, bs)(_ :: _)
  }

  /** [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/parallelism/5.answer.scala answer]] */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (pa, pb) =>
      map2(pa, pb)(_ :: _)
    }

  def parMapUsingSequence[A,B](l: List[A])(f: A => B): Par[List[B]] =
    fork {
      sequence(
        l map asyncF(f)
      )
    }
}
