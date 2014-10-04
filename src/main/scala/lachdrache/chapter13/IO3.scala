package lachdrache.chapter13

import lachdrache.chapter7.Par.Par

import scala.language.higherKinds

/**
 * We can generalize `TailRec` and `Async` to the type `Free`, which is
 * a `Monad` for any choice of `F`. */
object IO3 {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0,A]
  type Async[A] = Free[Par,A]

}
