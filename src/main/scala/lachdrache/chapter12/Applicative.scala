package lachdrache.chapter12

import lachdrache.chapter11.Functor
import lachdrache.chapter12.Applicative._
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

  // exercise c12/1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb) { (_, _) }

  // exercise c12/2 part 1: define in terms of map2 and unit
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { (a2b, a) =>
      a2b(a)
    }

  // exercise c12/2 part 2: define in terms of apply and unit
  def mapInTermsOfApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // exercise c12/2 part 3: define in terms of apply and unit
  def map2InTermsOfApplyAndUnit[A,B,C](fa :F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(
      mapInTermsOfApplyAndUnit(fa)(f.curried)
    )(fb)
  }

  // exercise c12/3
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D):F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E):F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // exercise c12/4: What is the meaning of streamApplicative.sequence
  object Exercise4 {
    // It turns a list of streams to a stream of lists.
    // The head of the result stream contains a List where the elements
    // are the heads of the input streams, e.g.
    val input = List(Stream(1, 2, 3), Stream(4, 5, 6))
    val result = streamApplicative.sequence(input)
    assert(result.take(3).toList == List(List(1, 4), List(2, 5), List(3, 6)))
    // More generally speaking is zips all of the input streams and converts
    // the nested tuples into a list.
    //
    // The authors have a very different way of explaining it:
    // This transposes the list! That is, we start with a list of rows,
    // each of which is possibly infinite in length. We get back a single row,
    // where each element is the column of values at that position.
  }

  def assoc[A,B,C](p: (A, (B, C))): ((A,B), C) =
    p match {
      case (a, (b, c)) => ((a, b), c)
    }

  def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  // exercise c12/7 TODO

  // exercise c12/8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))

      def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (fa, fb) match {
          case ((a0, a1), (b0, b1)) => (self.map2(a0,b0)(f), G.map2(a1,b1)(f))
        }
    }
  }


}

object Applicative {

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for (a <- fa; b <- fb) yield f(a,b)

    def unit[A](a: => A): Option[A] =
      Some(a)
  }

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map f.tupled

  }

  // exercise c12/6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E, A] =
      Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Failure(e1h, e1t), Failure(e2h, e2t)) => Failure(e1h, e1t ++ Vector(e2h) ++ e2t)
        case (Success(a), Success(b)) => Success(f(a,b))
        case (f@Failure(_, _), Success(_)) => f
        case (Success(_), f@Failure(_, _)) => f
      }
  }
}
