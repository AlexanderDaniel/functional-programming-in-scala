package lachdrache.chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMapGoingOverTheLiftTwice[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((z,a) => m.op(z, f(a)))

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero: String = ""
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1*a2
    val zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  val andBoolean = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def leftToRightEndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): (A) => A = a1 andThen a2
    override def zero: A => A = identity
  }

  def rightToLeftEndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): (A) => A = a1 compose a2
    override def zero: A => A = identity
  }

}
