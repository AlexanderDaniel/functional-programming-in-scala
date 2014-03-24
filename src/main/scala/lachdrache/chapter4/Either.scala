package lachdrache.chapter4

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B>:A](b: => Either[EE,B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C]
  def map2Validate[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[List[EE], C] = (this, b) match {
    case (Left(l1), Left(l2)) => Left(List(l1, l2))
    case (Left(l1), Right(r2)) => Left(List(l1))
    case (Right(r1), Left(l2)) => Left(List(l2))
    case (Right(r1), Right(r2)) => Right(f(r1, r2))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] =
    this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] =
    this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] =
    b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] =
    this
}

case class Right[+A](value: A) extends Either[Nothing, A]{
  override def map[B](f: (A) => B): Either[Nothing, B] =
    Right(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] =
    f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b map (bValue => f(value, bValue))
}

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.map2(sequence(t)) { (hr, tr) => hr :: tr }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f)) { (hr, tr) => hr :: tr }
  }

  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)
}
