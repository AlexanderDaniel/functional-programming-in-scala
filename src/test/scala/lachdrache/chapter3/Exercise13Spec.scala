package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.List._

/**
 * foldLeft: f(z, f(1, f(2, 3)))
 * foldRight: f(1, f(2, f(3, z)))
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

    def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b: B) => b){(a,z) =>
        b => z(f(b, a))
      }(z)

    def visualize[A](l: List[A]): String =
      foldLeftWithFoldRight(l, "0")((z,a) => s"f($z,$a)")

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
}