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

  def oneTo(n: Int): List[Int] = {
    @annotation.tailrec
    def go(n: Int, acc: List[Int]): List[Int] = n match {
      case 0 => acc
      case _ => go(n-1, Cons(n, acc))
    }
    go(n, Nil)
  }

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

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, z) => z+1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z:B)(f: (B,A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def productLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_*_)

  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((z, _) => z+1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((z, a) => Cons(a, z))

  def appendPatternMatching[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(a, as) => Cons(a, appendPatternMatching(as, a2))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2){(a, z) =>
      Cons(a, z)
    }

}
