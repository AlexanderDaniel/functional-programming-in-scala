package lachdrache.chapter11

import lachdrache.chapter8.Gen

import scala.language.higherKinds

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A):F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A):Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }
}