package lachdrache.chapter11

import org.scalatest.FunSpec
import Monad._

class MonadSpec extends FunSpec {

  // exercise 5
  // How will replicateM behave for various choices of F?
  // How for the List monad?
  //   replicate the List and flatten it
  describe("replicateM") {
    it("should replicate the list M times and flatten it") {
      assertResult(List(List(1,1,1))) {
        listMonad.replicateM(3, List(1))
      }
    }

    // LOOK WHAT IS HAPPENING HERE!
    it("should replicate multiples values and flatten it") {
      assertResult(List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))) {
        listMonad.replicateM(2, List(1,2))
      }
    }
    it("List.fill") {
      assertResult(List(List(1,2), List(1,2))) {
        List.fill(2)(List(1,2))
      }
    }

    it("substituting replicateM") {
      assertResult(List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))) {
        listMonad.sequence(List(List(1,2), List(1,2)))
      }
    }
    it("substituting sequence") {
      assertResult(List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))) {
        List(List(1,2), List(1,2)).foldRight(listMonad.unit(List.empty[Int]))((ma, mla) =>
          listMonad.map2(ma, mla)(_ :: _)
        )
      }
    }
    it("substituting first step of foldRight: inner") {
      assertResult(List(List(1), List(2))) {
        listMonad.map2(List(1, 2), List(List.empty[Int]))(_ :: _)
      }
    }
    it("substituting first step of foldRight: full") {
      assertResult(List(List(1), List(2))) {
        List(List(1,2)).foldRight(listMonad.unit(List.empty[Int]))((ma, mla) =>
          listMonad.map2(ma, mla)(_ :: _)
        )
      }
    }
    it("substituting second step of foldRight: inner") {
      assertResult(List(List(1,1), List(1,2), List(2,1), List(2,2))) {
        listMonad.map2(List(1, 2), List(List(1), List(2)))(_ :: _)
      }
    }

    it("should replicate the value and flatten it") {
      assertResult(Some(List('a', 'a', 'a'))) {
        optionMonad.replicateM(3, Some('a'))
      }
    }

    it("should yield None") {
      assertResult(None) {
        optionMonad.replicateM(3, None)
      }
    }

    // Why does the list simply disappear in this example
    it("list fill") {
      assertResult(List(None, None, None)) {
        List.fill(3)(None)
      }
    }
    it("sequence on list of none") {
      assertResult(None) {
        optionMonad.sequence(List(None, None, None))
      }
    }
    // Ah. It is the option monad in this case.
    // So a list of monads will be converted in a monad containing a list
    // def sequence[A](lma: List[F[A]]): F[List[A]]
    // So it will be None, but None cannot hold any value. Therefore is will be None.
    // def sequence[A](lma: List[Option[A]]): Option[List[A]]
    // ==>
    // lma.foldRight(unit(List.empty[A]))((ma, mla) =>
    //   map2(ma, mla)(_ :: _)
    // )
    // ==>
    // List(None, None, None).foldRight(List.empty[Option[Nothing])((ma, mla) =>
    //   map2(ma, mla)(_ :: _)
    // )
    // ==>
    // map2(None, Nil)(_ :: _)
    // ==>
    // flatMap(ma)(a => map(mb)(b => f(a,b)))
    // ==>
    // flatMap(None)(a => map(mb)(b => f(a,b)))
    // ==>
    // None

    it("sequence on list monad starting with None will always yield None") {
      assertResult(None) {
        optionMonad.sequence(List(None, Some(1), Some(2)))
      }
    }
    it("list with two monads") {
      assertResult(Some(List(1,2))) {
        optionMonad.sequence(List(Some(1), Some(2)))
      }
    }

  }

  describe("product should convert a pair of monads to a monad of pairs") {
    it("optionMonad with None at first position") {
      assertResult(None) {
        optionMonad.product(None, Some(3))
      }
    }
    it("optionMonad with None at second position") {
      assertResult(None) {
        optionMonad.product(Some(1), None)
      }
    }
    it("optionMonad with Some") {
      assertResult(Some(1,3)) {
        optionMonad.product(Some(1), Some(3))
      }
    }

    it("listMonad with empty lists") {
      assertResult(Nil) {
        listMonad.product(Nil, Nil)
      }
    }
    it("listMonad with two singleton lists") {
      assertResult(List((1,2))) {
        listMonad.product(List(1), List(2))
      }
    }
    it("listMonad with one singleton list and one two-element list") {
      assertResult(List((1,2), (1,3))) {
        listMonad.product(List(1), List(2,3))
      }
    }
    it("listMonad with two two-element lists") {
      assertResult(List((1,3), (1,4), (2,3), (2,4))) {
        listMonad.product(List(1,2), List(3,4))
      }
    }
  }
}
