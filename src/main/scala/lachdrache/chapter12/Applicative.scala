package lachdrache.chapter12

import lachdrache.chapter11.Functor
import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa :F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, fb) =>
      map2(f(a), fb)(_ :: _)
    }

  // exercise 1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb) { (_, _) }

  // exercise 2 part 1: define in terms of map2 and unit
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab) { (a, a2b) =>
      a2b(a)
    }

  // exercise 2 part 2: define in terms of apply and unit
  def mapInTermsOfApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // exercise 2 part 3: define in terms of apply and unit
  def map2InTermsOfApplyAndUnit[A,B,C](fa :F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(
      mapInTermsOfApplyAndUnit(fa)(f.curried)
    )(fb)
  }

  // exercise 3
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D):F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E):F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)


}
