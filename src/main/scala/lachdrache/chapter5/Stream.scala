package lachdrache.chapter5

import scala.collection.mutable.ListBuffer
import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailrec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h,t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }

  def toListWithBuffer: List[A] = {
    val lb = ListBuffer.empty[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => lb.toList
      case Cons(h,t) =>
        lb.append(h())
        go(t())
    }
    go(this)
  }

  def take(n: Int): Stream[A] =
    if (n<=0) Empty
    else this match {
      case Empty => throw new NoSuchElementException
      case Cons(h,t) => Cons(h, () => t().take(n-1))
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    if (n<=0) this
    else this match {
      case Empty => throw new NoSuchElementException
      case Cons(_, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) =>
      val head = h()
      if (p(head)) cons(head, t().takeWhile(p))
      else Empty
  }

  @annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, z) => p(a) || z)

  def forall(p: A => Boolean): Boolean =
    foldRight(true)((a,z) => p(a) && z)

  def takeWhileViaFoldRight(p: A => Boolean) =
    foldRight(empty[A])((a,z) => if (p(a)) cons(a,z) else empty)

  def headOption = foldRight(None: Option[A])((a,_) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,z) => cons(a,z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a,z) =>
      f(a) append z
    }

  def find(p: A=>Boolean): Option[A] =
    filter(p).headOption
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def from(n :Int): Stream[Int] =
    cons(n, from(n+1))

  val naturalNumbers = from(1)

  val fibs = {
    def fib(n0: Int, n1: Int):Stream[Int] = {
      cons(n0, fib(n1, n0+n1))
    }
    fib(1,1)
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = cons(a, stream)
    stream
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }

  }

}