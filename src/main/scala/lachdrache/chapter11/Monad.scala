package lachdrache.chapter11

import lachdrache.chapter7.Par
import lachdrache.chapter7.Par.Par
import lachdrache.chapter8.Gen

import scala.language.higherKinds

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A):F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  // exercise 3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) =>
      map2(ma, mla)(_ :: _)
    )
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)

  // exercise 4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
    map2(ma, mb)((_, _))

  // exercise 6
  def filterMByLachdrache[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val mlla: F[List[List[A]]] = sequence(ms map { a =>
      filterA(a)(f)
    })
    map(mlla)(_.flatten)
  }
  def filterA[A](a: A)(f: A => F[Boolean]): F[List[A]] =
    map(f(a))(b => if (b) List(a) else List.empty[A])
  
  // exercise 6 (author's solution)
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  // exercise 7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // exercise 8
  def flatMapInTermsOfCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A):Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // exercise 1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A):Par[A] = Par.unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A):Option[A] = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A):Stream[A] = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }
  val listMonad = new Monad[List] {
    def unit[A](a: => A):List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }
}