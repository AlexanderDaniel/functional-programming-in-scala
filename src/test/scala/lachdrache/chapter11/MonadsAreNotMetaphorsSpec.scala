package lachdrache.chapter11

import org.scalatest.FunSpec
import scala.language.higherKinds

/**
 * Coding while reading http://www.codecommit.com/blog/ruby/monads-are-not-metaphors
 */
class MonadsAreNotMetaphorsSpec extends FunSpec {

  describe("combining lambdas with andThen") {
    def foo(bar: String): Int = {
      (((u: Unit) => println(bar)) andThen ((u: Unit) => bar.length))()
    }

    it("should return the length") {
      assertResult(4) {
        foo("alex")
      }
    }
  }

  case class Thing[+A](value: A) {
    def bind[B](f: A => Thing[B]) = f(value)
  }

  describe("thing") {
    def foo(i: Int) = Thing(i + 1)

    it("flatMap aka bind") {
      assertResult(Thing(4)) {
        Thing(3) bind foo
      }
    }
  }

  sealed trait Option[+A] {
    def bind[B](f: A => Option[B]): Option[B]
  }
  case class Some[+A](value: A) extends Option[A] {
    def bind[B](f: (A) => Option[B]): Option[B] = f(value)
  }
  case object None extends Option[Nothing] {
    def bind[B](f: (Nothing) => Option[B]): Option[B] = this
  }

  // typeclass
  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A,B](m: M[A])(f: A => M[B]): M[B]
  }
  implicit object ThingMonad extends Monad[Thing] {
    def unit[A](a: A): Thing[A] = Thing(a)
    def bind[A, B](thing: Thing[A])(f: (A) => Thing[B]): Thing[B] =
      thing bind f
  }
  implicit object OptionMonad extends Monad[Option] {
    def unit[A](a: A): Option[A] = Some(a)
    def bind[A, B](opt: Option[A])(f: (A) => Option[B]): Option[B] =
      opt bind f
  }
  def sequence[M[_], A](ms: List[M[A]])(implicit tc: Monad[M]) = {
    ms.foldRight(tc.unit(List[A]())) { (m, acc) =>
      tc.bind(m) { a => tc.bind(acc) { tail =>
        tc.unit(a :: tail)
      }}
    }
  }

  describe("sequence") {
    it("for option") {
      val nums: List[Option[Int]] = List(Some(1), Some(2), Some(3))
      assertResult(Some(List(1,2,3))) {
        sequence(nums)
      }
    }
    it("for thing") {
      val nums = List(Thing(1), Thing(2), Thing(3))
      assertResult(Thing(List(1, 2, 3))) {
        sequence(nums)
      }
    }
  }


}