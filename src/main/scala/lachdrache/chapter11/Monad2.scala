package lachdrache.chapter11

trait Monad2[F[_]] {
  // primitives
  def unit[A](a: => A):F[A]
  def join[A](mma: F[F[A]]): F[A]
  def map[A,B](ma: F[A])(f: A => B): F[B]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))
}

object Monad2 {
  val optionMonad = new Monad2[Option] {
    def unit[A](a: => A):Option[A] = Some(a)
    def join[A](mma: Option[Option[A]]): Option[A] = mma.getOrElse(None)
    def map[A, B](ma: Option[A])(f: (A) => B): Option[B] =
      ma map f
  }
}
