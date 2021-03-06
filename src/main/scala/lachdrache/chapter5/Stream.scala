package lachdrache.chapter5

import scala.collection.mutable.ListBuffer
import Stream._
import scala.language.postfixOps

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

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (_, i) if i<=0 => None
      case (Empty, _) => None
      case (Cons(h,t), i) => Some((h(), (t(), i-1)))
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

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
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

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

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

  def zipWith[B,C](bs: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Empty, _) | (_, Empty) => None
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
    }

  def zip[B](bs: Stream[B]): Stream[(A,B)] =
    zipWith(bs)((_,_))

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    def tail[T](s: Stream[T]) = s match {
      case Empty => Empty
      case Cons(_, t) => t()
    }
    unfold((this, bs)) {
      case (s1, s2) => Some((s1.headOption, s2.headOption), (tail(s1), tail(s2)))
    }
  }

  def zipAll2[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    val s1 = this map (Some(_)) append constant(None)
    val s2 = bs map (Some(_)) append constant(None)
    s1 zip s2
  }

  def startsWith[B >: A](s2: Stream[B]): Boolean = (this, s2) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1()==h2() => t1().startsWith(t2())
    case _ => false
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some((s, t()))
    } append Stream(Stream())

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def reverse: Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A]): Stream[A] = s match {
      case Empty => acc
      case Cons(h,t) => go(t(), cons(h(), acc))
    }
    go(this, Empty)
  }

  def scanRightWithReverse[B](z: B)(f: (A,B) => B): Stream[B] =
    cons(z,unfold((z, this.reverse)) {
      case (_, Empty) => None
      case (z0, Cons(h,t)) =>
        val z1 = f(h(), z0)
        Some((z1, (z1, t())))
    }).reverse

  def head: A = this match {
    case Cons(h, _) => h()
    case _ => throw new NoSuchElementException("No head in empty stream")
  }

  def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] =
    foldRight(Stream(z)) { (a,z) =>
      cons(f(a,z.head), z)
    }

  /** The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
    * It can be implemented using `foldRight` though.
    *
    * The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
    * which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than
    * is needed to compute the result. Here, we simply extract the accumulated list once finished.
    */
  def scanRightAnswer[B](z: B)(f: (A,=>B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    }) _2

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

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a,s)) => cons(a, unfold(s)(f))
  }

  def startsWith2[A](s1: Stream[A], s: Stream[A]): Boolean =
    s1.zipAll(s).takeWhile(!_._2.isEmpty) forall {
      case (Some(h),Some(h2)) if h == h2 => true
      case _ => false
    }
}