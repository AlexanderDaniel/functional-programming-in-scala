package lachdrache.chapter13

import lachdrache.chapter11.Monad
import lachdrache.chapter7.Par.Par

import scala.language.higherKinds

/**
 * We can generalize `TailRec` and `Async` to the type `Free`, which is
 * a `Monad` for any choice of `F`. */
object IO3 {

  sealed trait Free[F[_], A] {
    // Exercise 13.1 part 1
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      this flatMap (a => Return(f(a)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  // Exercise 13.1 part 2
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      def unit[A](a: => A): Free[F, A] =
        Return(a)
      def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] =
        ma flatMap f
    }

  // Exercise 13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(t, g) => runTrampoline(t flatMap (a => g(a) flatMap f))
    }
  }

  // Exercise 13.3
  /** return either a `Suspend`, a `Return`, or a right-associated `FlatMap` */
  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }
  @annotation.tailrec
  def step[F[_], A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

}
