package lachdrache.chapter12

import lachdrache.chapter11.Monad
import lachdrache.chapter12.Traverse.Tree
import org.scalatest.FunSpec

class TraverseSpec extends FunSpec {

  describe("List[Option[A]] => Option[List[A]] with Applicative") {
    it("this is the standard applicative sequence") {
      assertResult(Some(List(1, 2, 3))) {
        Applicative.optionApplicative.sequence(List(Some(1), Some(2), Some(3)))
      }
    }
  }

  describe("List[Option[A]] => Option[List[A]] with Traverse") {
    implicit val optionApplicative = Applicative.optionApplicative

    it("The types: Traverse is List, Applicative is Option") {
      assertResult(Some(List(1, 2, 3))) {
        Traverse.listTraverse.sequence(List[Option[Int]](Some(1), Some(2), Some(3))) // type hint is needed
      }
    }
  }

  describe("Option[List[A]] => List[Option[A]] • The types: Traverse is Option, Applicative is List") {
    implicit val applicative = Monad.listMonad

    it("Some") {
      assertResult(List(Some(1), Some(2), Some(3))) {
        Traverse.optionTraverse.sequence(Some(List(1, 2, 3)))
      }
    }

    it("None") {
      assertResult(List(None)) {
        Traverse.optionTraverse.sequence(None)
      }
    }
  }

  describe("Tree[Option[A]] => Option[Tree[A]]") {
    implicit val optionApplicative = Applicative.optionApplicative

    val treeWithNoChildren: Tree[Option[Int]] = Tree(Some(1))
    val treeWithThreeChildren: Tree[Option[Int]] = treeWithNoChildren.copy(tail = List(Tree(Some(11)), Tree(Some(12)), Tree(Some(13))))
    val threeLevelTree: Tree[Option[Int]] = Tree(Some(0), List(Tree(Some(1), List(Tree(Some(2))))))

    it("Tree(Some(1)) => Some(Tree(1))") {
      assertResult(Some(Tree(1))) {
        Traverse.treeTraverse.sequence(treeWithNoChildren)
      }
    }

    it("should swap the boxes for a two level tree") {
      assertResult(Some(Tree(1, List(Tree(11), Tree(12), Tree(13))))) {
        Traverse.treeTraverse.sequence(treeWithThreeChildren)
      }
    }

    it("should work for a three level tree") {
      assertResult(Some(Tree(0, List(Tree(1, List(Tree(2))))))) {
        Traverse.treeTraverse.sequence(threeLevelTree)
      }
    }

    it("None is stronger than Some even though it can hold a value") {
      assertResult(None) {
        Traverse.treeTraverse.sequence(Tree(None))
      }
    }

    it("if None is one of the children the whole family is None (feared by parents)") {
      assertResult(None) {
        Traverse.treeTraverse.sequence(Tree(Some("1"), List(Tree(Some("1a")), Tree(None))))
      }
    }

    it("even the grand-child can doom the whole family (FP knows it all)") {
      assertResult(None) {
        Traverse.treeTraverse.sequence(Tree(Some("a"), List(Tree(Some("b"), List(Tree(None))))))
      }
    }
  }

}
