package lachdrache.chapter13

import lachdrache.chapter7.Par.Par

object IO2c {

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]) =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]
}
