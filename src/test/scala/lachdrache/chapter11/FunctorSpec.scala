package lachdrache.chapter11

import org.scalatest.FunSpec
import Functor._

class FunctorSpec extends FunSpec {

  describe("distribute") {
    it("should work for list functor") {
      assertResult((List(1, 2, 3), List(3, 2, 1))) {
        listFunctor.distribute(List((1, 3), (2, 2), (3, 1)))
      }
    }
  }

  describe("codistribute") {
    it("should work for list functor") {
      assertResult()
    }
  }
}
