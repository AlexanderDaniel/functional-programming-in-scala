package lachdrache.chapter11

import lachdrache.chapter12.Applicative
import lachdrache.chapter6.State
import lachdrache.chapter7.Par
import lachdrache.chapter7.Par.Par
import lachdrache.chapter8.Gen

import scala.language.higherKinds
import scala.language.implicitConversions

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A):F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  // exercise 3
  override def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) =>
      map2(ma, mla)(_ :: _)
    )
  override def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)

  // exercise 4
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  override def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
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

  // exercise 6 by Yago
  def filterMByYago[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]())) { (a, fla) =>
      map2(f(a), fla)((b, la) => if (b) a :: la else la)
    }

  // exercise 7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // exercise 8
  def flatMapInTermsOfCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  // exercise 12
  // https://github.com/fpinscala/fpinscala/issues/289
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  // exercise 13
  def composeInTermsOfJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
  def flatMapInTermsOfJoin[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)
  def skip[A](a: F[A]): F[Unit] = as(a)(())

  /** Folds the stream with the function f, combing the effects and returning the result */
  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => flatMap(f(z, h))(z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  /** The same as the `foldM` function above except ignores the result */
  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    skip(
      foldM(l)(z)(f)
    )

  /** Calls the function f for each element of the stream and combines the effects */
  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u,a) => skip(f(a)))

  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)
  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)

  // syntax
  implicit def toMonadic[A](a: F[A]): Monadic[F,A] =
    new Monadic[F,A] { val F = Monad.this; def get = a }
}

trait Monadic[F[_],A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def **[B](b: F[B]) = F.map2(a,b)((_,_))
  def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
  def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
  def replicateM(n: Int) = F.replicateM(n, a)
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

  // exercise 17
  case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] =
      f(value)
    def map[B](f: A => B): Id[B] =
      idMonad.map(this)(f)
  }
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
      ma flatMap f
  }

  type IntState[A] = State[Int, A]
  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))

    def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] =
      ma flatMap f
  }
  object IntStateMonadWithAnonymousType extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
    def unit[A](a: => A): State[Int, A] = State(s => (a, s))

    def flatMap[A, B](ma: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] =
      ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      ma flatMap f
  }

  // exercise c12/5
  def eitherMonad[E]: Monad[({type f[x] = Either[E,x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      ma.fold(
        lv => Left(lv),
        rv => f(rv)
      )

    def unit[A](a: => A): Either[E, A] = Right(a)
  }

}