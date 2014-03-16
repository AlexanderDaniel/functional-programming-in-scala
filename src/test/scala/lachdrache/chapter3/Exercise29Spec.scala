package lachdrache.chapter3

import org.specs2.mutable.Specification
import lachdrache.chapter3.Tree._

/**
 * For fold we have to consider the classes [[Branch]] and [[Leaf]].
 * `map`, `size` and `maximum` would be fine just with [[Leaf]].
 * But `depth` also needs [[Branch]].
 *
 * Hence for [[Leaf]] we have following signature: `(z: B, a: A) => B`
 *
 * For [[Branch]] we have following signature: `(z: B, left: B, right: B) => B`
 *
 * But in fact we need no initial value `z` since we cannot have an empty tree
 *
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/29.hint.txt hint]]
 * and
 * [[https://github.com/pchiusano/fpinscala/blob/master/answerkey/datastructures/29.answer.scala answer]]
 */
class Exercise29Spec extends Specification {

  "fold" should {
    {
      fold(Branch(Leaf('a'), Leaf('b')))(_.toString)((l, r) => s"[$l,$r]") === "[a,b]"
    }.eg

    "same with block expressions" in {
      fold(Branch(Leaf('a'), Leaf('b'))) {
        _.toString
      } {
        (l, r) => s"[$l,$r]"
      } === "[a,b]"
    }
    "more branches" in {
      fold(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))) {
        _.toString
      } {
        (l, r) => s"[$l,$r]"
      } === "[a,[b,c]]"
    }
  }

  "size via fold" should {
    def size[A](tree: Tree[A]): Int =
      fold(tree) {
        _ => 1
      } {
        1+_+_
      }

    {
      size(Leaf('a')) === 1
    }.eg

    {
      size(Branch(Leaf('a'), Leaf('b'))) === 3
    }.eg

    {
      size(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))) === 5
    }.eg
  }

  "maximum via fold" should {

    def maximum(tree: Tree[Int]): Int =
      fold(tree)(identity)(_ max _)

    {
      maximum(Leaf(13)) === 13
    }.eg

    {
      maximum(Branch(Leaf(13), Leaf(1))) === 13
    }.eg

    {
      maximum(Branch(Leaf(12), Branch(Leaf(13), Leaf(7)))) === 13
    }.eg
  }

  "depth via fold" should {

    def depth[A](tree: Tree[A]): Int =
      fold(tree)(_ => 0)((l,r) => 1 + (l max r))

    {
      depth(Leaf('a')) === 0
    }.eg

    {
      depth(Branch(Leaf('a'), Leaf('b'))) === 1
    }.eg

    {
      depth(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))) === 2
    }.eg
  }

  "map via fold" should {

    def map[A,B](tree: Tree[A])(f: A=>B): Tree[B] =
      fold[A, Tree[B]](tree) {
        a => Leaf(f(a))
      } {
        Branch(_, _)
      }

    {
      map(Leaf(3))(_+10) === Leaf(13)
    }.eg

    {
      map(Branch(Leaf(1), Leaf(2)))(_+10) === Branch(Leaf(11), Leaf(12))
    }.eg

    {
      map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(x => -x) === Branch(Leaf(-1), Branch(Leaf(-2), Leaf(-3)))
    }.eg

  }
}