package lachdrache.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_+_)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_*_)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], e: A) =
    Cons(e, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n-1)
  }

  /**
   * More elegant way: https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/5.answer.scala
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => throw new UnsupportedOperationException
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

}
