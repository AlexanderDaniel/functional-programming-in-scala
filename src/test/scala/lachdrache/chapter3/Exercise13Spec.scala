package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/13.hint.txt hint]]
 * and
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/13.answer.scala answer]]
 */
class Exercise13Spec extends Specification {

  "visualize foldLeft" should {
    def visualize[A](l: List[A]): String =
      foldLeft(l, "0")((z,a) => s"f($z,$a)")

    {
      visualize(Nil) === "0"
    }.eg

    {
      visualize(List(1)) === "f(0,1)"
    }.eg

    {
      visualize(List(1,2,3)) === "f(f(f(0,1),2),3)"
    }.eg
  }
  
  "visualize foldRight" should {
    def visualize[A](l: List[A]): String =
      foldRight(l, "0")((a,z) => s"f($a,$z)")
  
    {
      visualize(Nil) === "0"
    }.eg

    {
      visualize(List(1)) === "f(1,0)"
    }.eg

    {
      visualize(List(1,2,3)) === "f(1,f(2,f(3,0)))"
    }.eg
  }
  
  "foldLeft in terms of foldRight" should {

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b: B) => b){(a,z) =>
        b => z(f(b, a))
      }(z)

    def visualize[A](l: List[A]): String =
      foldLeftViaFoldRight(l, "0")((z,a) => s"f($z,$a)")

    {
      visualize(Nil) === "0"
    }.eg

    {
      visualize(List(1)) === "f(0,1)"
    }.eg

    {
      visualize(List(1,2,3)) === "f(f(f(0,1),2),3)"
    }.eg

  }

  "foldRight in terms of foldLeft" should {
    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(l, (b: B) => b){(z,a) =>
        b => z(f(a, b))
      }(z)

    def visualize[A](l: List[A]): String =
      foldRightViaFoldLeft(l, "0")((a,z) => s"f($a,$z)")

    {
      visualize(Nil) === "0"
    }.eg

    {
      visualize(List(1)) === "f(1,0)"
    }.eg

    {
      visualize(List(1,2,3)) === "f(1,f(2,f(3,0)))"
    }.eg

    "but still throws StackOverflowError for large lists" in {
      def length[A](l: List[A]) = foldRightViaFoldLeft(l,0)((_,z) => z+1)
      length(oneTo(10000)) should throwA[StackOverflowError]
      // probably because of the huge function chain which is built
      // f(1, f(2, f(3, f(4, ....
      // and which has to be evaluated.
      // But are function arguments evaluated on a own stack frame?
    }
  }

  "foldRight in terms of foldLeft and reverse" should {
    def foldRightViaFoldLeftAndReverse[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(reverse(l), z)((z,a) => f(a,z))

    def visualize[A](l: List[A]): String =
      foldRightViaFoldLeftAndReverse(l, "0")((a,z) => s"f($a,$z)")

    {
      visualize(Nil) === "0"
    }.eg

    {
      visualize(List(1)) === "f(1,0)"
    }.eg

    {
      visualize(List(1,2,3)) === "f(1,f(2,f(3,0)))"
    }.eg

    "work for large lists" in {
      def length[A](l: List[A]) = foldRightViaFoldLeftAndReverse(l,0)((_,z) => z+1)
      length(oneTo(10000)) === 10000
    }
  }
}